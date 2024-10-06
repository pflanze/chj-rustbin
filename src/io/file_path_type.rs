use std::path::Path;
use std::{fmt::Debug, ffi::OsString, path::PathBuf};
use std::fs;

use anyhow::{anyhow, Result, Context};
use genawaiter::rc::Gen;
use log::trace;

use crate::io::excludes::Excludes;
use crate::region::{RegionId, Region};
use crate::scope;


/// Get filesystem items while making use of the excludes module.


#[derive(Debug, Clone, Copy)]
pub struct ItemOptions {
    pub dirs: bool,
    pub files: bool,
    pub other: bool,
}

/// Implement conversion from the type `$from` to
/// `ItemOptions`. Assumes the `dirs`, `files`, and `other` fields are
/// present on `$from` as `bool`.
#[macro_export]
macro_rules! impl_item_options_from {
    { $from:ty } => {
        impl From<&$from> for $crate::io::file_path_type::ItemOptions {
            fn from(o: &$from) -> Self {
                $crate::io::file_path_type::ItemOptions {
                    dirs: o.dirs,
                    files: o.files,
                    other: o.other
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileType {
    /// `File` does not include symlinks
    File,
    Dir,
    Symlink,
    Other
}

impl FileType {
    pub fn is_dir(self) -> bool {
        match self {
            FileType::Dir => true,
            _ => false
        }
    }
    /// Does not include symlinks
    pub fn is_file(self) -> bool {
        match self {
            FileType::File => true,
            _ => false
        }
    }
}

impl From<&fs::FileType> for FileType {
    fn from(ft: &fs::FileType) -> Self {
        if ft.is_dir() { Self::Dir }
        else if ft.is_file() { Self::File }
        else if ft.is_symlink() { Self::Symlink }
        else { Self::Other }
    }
}

/// This trait is used to create parent nodes while walking
/// directories/trees. You need to implement it for your type (it is
/// already implemented for `PathBuf`).
pub trait FileParent<'region>: Sized {
    fn new(
        region: &Region<'region, Self>,
        item: &FilePathType<'region, Self>
    ) -> Self;
    fn path(&self) -> &Path;
}

impl<'region> FileParent<'region> for PathBuf {
    fn new(
        region: &Region<'region, Self>,
        item: &FilePathType<'region, Self>
    ) -> Self {
        region.get(item.file_parent).clone().join(&item.file_name)
    }

    fn path(&self) -> &Path {
        self
    }
}


#[derive(Debug, PartialEq)]
pub struct FilePathType<'region, P: FileParent<'region> = PathBuf> {
    pub file_parent: RegionId<'region, P>,
    pub file_name: OsString,
    pub file_type: FileType,
}

// Why do we need to impleemnt Clone manually, when RegionId already
// does it, and P is the type about which the compiler complains; is
// derive(Clone) intentionally restricting it?
impl<'region, P: FileParent<'region>> Clone for FilePathType<'region, P> {
    fn clone(&self) -> Self {
        Self {
            file_parent: self.file_parent,
            file_name: self.file_name.clone(),
            file_type: self.file_type
        }
    }
}

impl<'region, P: FileParent<'region>> FilePathType<'region, P> {
    pub fn is_file(&self) -> bool {
        self.file_type.is_file()
    }
    pub fn is_dir(&self) -> bool {
        self.file_type.is_dir()
    }
    pub fn to_path_buf(&self, region: &Region<'region, P>) -> PathBuf {
        // XX cache value?
        region.get(self.file_parent).path().join(&self.file_name)
    }
}

/// Does not descend into dirs. You'll want to use `PathBuf` for `P`
/// unless you have a need to store additional data in the parent
/// nodes.
pub fn file_path_types_iter<'region, 't, P: FileParent<'t>>(
    region: &'t Region<'region, P>,
    file_parent: RegionId<'region, P>,
    opt: ItemOptions,
    excludes: &'t Excludes
) -> Result<Box<dyn Iterator<Item = Result<FilePathType<'t, P>>> + 't>> {
    // eprintln!("items({dir_path:?}, {opt:?})");
    let iterator = scope!{ fs::read_dir(region.get(file_parent).path()) }.with_context(
        || anyhow!("opening directory {:?} for reading", region.get(file_parent).path()))?
    .filter_map(
        move |entry_result: Result<fs::DirEntry, std::io::Error>|
                                   -> Option<Result<FilePathType<P>>> {
            match entry_result {
                Ok(entry) => {
                    let ft = entry.file_type()
                        .expect("does this fail on OSes needing stat?");
                    let file_name = entry.file_name();
                    let handle_as_dir = ft.is_dir()
                        && opt.dirs
                        && ! excludes.filename_is_excluded(&file_name, true);
                    let handle_as_file = ft.is_file()
                        && opt.files
                        && ! excludes.filename_is_excluded(&file_name, false);
                    let handle_as_other = opt.other &&
                        (!ft.is_dir() && !ft.is_file());
                    if handle_as_dir || (
                        handle_as_file || handle_as_other) {
                        let file_type = FileType::from(&ft);
                        Some(Ok(FilePathType { file_parent, file_name, file_type }))
                    } else {
                        trace!(
                            "ignoring item '{:?}' (type {:?})",
                            file_name, ft);
                        None
                    }
                },
                Err(e) =>
                    Some(Err(e).with_context(|| anyhow!("read_dir on {:?}",
                                                        region.get(file_parent).path())))
            }
        });
    Ok(Box::new(iterator))
}

/// Does not descend into dirs.
pub fn file_path_types_vec<'region, 't, P: FileParent<'t>>(
    region: &'t Region<'region, P>,
    file_parent: RegionId<'region, P>,
    opt: ItemOptions,
    excludes: &'t Excludes
) -> Result<Vec<FilePathType<'t, P>>> {
    file_path_types_iter(region, file_parent, opt, excludes)?.collect::<Result<_,_>>()
}

/// Descends into subdirs. You'll want to use `PathBuf` for `P`
/// unless you have a need to store additional data in the parent
/// nodes.
pub fn recursive_file_path_types_iter<'region, 't, P: FileParent<'t>>(
    region: &'t Region<'region, P>,
    file_parent: RegionId<'region, P>,
    opt: ItemOptions,
    excludes: &'t Excludes
) -> Box<dyn Iterator<Item = Result<FilePathType<'t, P>>> + 't> {
    Box::new(Gen::new(|co| async move {
        let orig_opt = opt;
        let opt_with_dir = ItemOptions {
            dirs: true,
            ..orig_opt
        };
        match file_path_types_iter(region, file_parent, opt_with_dir, excludes) {
            Ok(mut iter) => {
                let mut stack = vec![];
                loop {
                    while let Some(item) = iter.next() {
                        match item {
                            Ok(item) => {
                                if item.is_dir() {
                                    let file_parent = region.store(P::new(region, &item));
                                    if orig_opt.dirs {
                                        co.yield_(Ok(item.clone())).await;
                                    }
                                    stack.push(iter);
                                    match file_path_types_iter(
                                        region, file_parent, opt_with_dir, excludes) {
                                            Ok(new_iter) => iter = new_iter,
                                        Err(e) => {
                                            co.yield_(Err(e)).await;
                                            return;
                                        }
                                    }
                                } else {
                                    co.yield_(Ok(item)).await;
                                }
                            }
                            Err(e) => {
                                co.yield_(Err(e)).await;
                                return;
                            }
                        }
                    }
                    if let Some(old_iter) = stack.pop() {
                        iter = old_iter;
                    } else {
                        return;
                    }
                }
            }
            Err(e) => {
                co.yield_(Err(e)).await;
                return;
            }
        }
    }).into_iter())
}

#[cfg(test)]
mod tests {
    use crate::io::excludes::empty_excludes;

    use super::*;

    #[test]
    fn t_recursive_file_path_types_iter() {
        let region: Region<PathBuf> = Region::new();
        let excludes = empty_excludes(true);
        let t = |opt| -> Result<Vec<PathBuf>, > {
            let iter = recursive_file_path_types_iter(
                &region,
                region.store("test/file_path_type/".into()),
                opt,
                &excludes);
            let mut v = iter.map(|r| r.map(|s| s.to_path_buf(&region)))
                .collect::<Result<Vec<_>, _>>()?;
            v.sort_by(|a, b| a.cmp(&b));
            Ok(v)
        };

        assert_eq!(
            t(
                ItemOptions {
                    dirs: true,
                    files: false,
                    other: true,
                }
            ).unwrap(),
            &[
                "test/file_path_type/bar",
                "test/file_path_type/foo"
            ].map(PathBuf::from));

        assert_eq!(
            t(
                ItemOptions {
                    dirs: true,
                    files: true,
                    other: true,
                }
            ).unwrap(),
            &[
                "test/file_path_type/bar",
                "test/file_path_type/bar/c",
                "test/file_path_type/foo",
                "test/file_path_type/foo/a",
                "test/file_path_type/foo/b"
            ].map(PathBuf::from));
                   
        assert_eq!(
            t(
                ItemOptions {
                    dirs: false,
                    files: true,
                    other: true,
                }
            ).unwrap(),
            &[
                "test/file_path_type/bar/c",
                "test/file_path_type/foo/a",
                "test/file_path_type/foo/b"
            ].map(PathBuf::from));
                   
    }
}
