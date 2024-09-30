/// Get filesystem items while making use of the excludes module.

use std::{fmt::Debug, ffi::OsString, path::PathBuf};
use std::fs;

use anyhow::{anyhow, Result, Context};
use log::trace;

use crate::excludes::{Excludes, generic_ignore_filename};


#[derive(Debug, Clone, Copy)]
pub struct ItemOptions {
    pub all: bool,
    pub dirs: bool,
    pub files: bool,
    pub other: bool,
}

/// Implement conversion from the type `$from` to
/// `ItemOptions`. Assumes the `all`, `dirs`, `files`, and `other`
/// fields are present on `$from` as `bool`.
#[macro_export]
macro_rules! impl_item_options_from {
    { $from:ty } => {
        impl From<&$from> for chj_rustbin::file_path_type::ItemOptions {
            fn from(o: &$from) -> Self {
                chj_rustbin::file_path_type::ItemOptions {
                    all: o.all,
                    dirs: o.dirs,
                    files: o.files,
                    other: o.other
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum FileType {
    File,
    Dir,
    Other
}

impl From<&fs::FileType> for FileType {
    fn from(ft: &fs::FileType) -> Self {
        if ft.is_dir() { Self::Dir }
        else if ft.is_file() { Self::File }
        else { Self::Other }
    }
}

pub struct FilePathType<'t> {
    pub dir_path: &'t PathBuf,
    pub file_name: OsString,
    pub file_type: FileType,
}

pub fn file_path_types_iter<'t>(dir_path: &'t PathBuf, opt: ItemOptions, excludes: &'t Excludes)
                  -> Result<Box<dyn Iterator<Item = Result<FilePathType<'t>>> + 't>> {
    // eprintln!("items({dir_path:?}, {opt:?})");
    let iterator = fs::read_dir(dir_path).with_context(
        || anyhow!("opening directory {dir_path:?} for reading"))?
    .filter_map(
        move |entry_result: Result<fs::DirEntry, std::io::Error>| -> Option<Result<FilePathType>> {
            match entry_result {
                Ok(entry) => {
                    let ft = entry.file_type()
                        .expect("does this fail on OSes needing stat?");
                    let file_name = entry.file_name();
                    if opt.all || ! generic_ignore_filename(&file_name) {
                        let handle_as_dir = ft.is_dir()
                            && opt.dirs
                            && !excludes.dirs.contains(&file_name);
                        let handle_as_file = ft.is_file()
                            && opt.files
                            && !excludes.files.contains(&file_name);
                        let handle_as_other = opt.other &&
                            (!ft.is_dir() && !ft.is_file());
                        if handle_as_dir || (
                            handle_as_file || handle_as_other) {
                            let file_type = FileType::from(&ft);
                            Some(Ok(FilePathType { dir_path, file_name, file_type }))
                        } else {
                            trace!(
                                "ignoring item '{:?}' (type {:?})",
                                file_name, ft);
                            None
                        }
                    } else {
                        None
                    }
                },
                Err(e) =>
                    Some(Err(e).with_context(|| anyhow!("read_dir on {dir_path:?}")))
            }
        });
    Ok(Box::new(iterator))
}

pub fn file_path_types_vec<'t>(dir_path: &'t PathBuf, opt: ItemOptions, excludes: &'t Excludes)
                           -> Result<Vec<FilePathType<'t>>> {
    file_path_types_iter(dir_path, opt, excludes)?.collect::<Result<_,_>>()
}
