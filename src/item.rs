/// Get filesystem items while making use of the excludes module.

use std::{fmt::Debug, ffi::OsString, time::SystemTime, path::{PathBuf, Path}};
use std::fs;

use anyhow::{anyhow, Result, Context};
use log::trace;

use crate::excludes::{Excludes, generic_ignore_filename};


#[derive(Debug)]
pub struct NoPath;

#[derive(Debug)]
pub struct Item<P: Debug> {
    pub parentdir: P,
    pub filename: OsString,
    pub mtime: SystemTime,
}

impl Item<NoPath> {
    pub fn with_parent(self, parentdir: PathBuf) -> Item<PathBuf> {
        Item {
            parentdir,
            filename: self.filename,
            mtime: self.mtime
        }
    }
}

pub fn newer_item<P: Debug>(
    a: Option<Item<P>>, b: Option<Item<P>>
) -> Option<Item<P>> {
    match (a, b) {
        (Some(a), Some(b)) => {
            if (a.mtime < b.mtime) ||
                ((a.mtime == b.mtime) && (a.filename > b.filename))
            {
                Some(b)
            } else {
                Some(a)
            }
        },
        (a, None) => a,
        (None, b) => b
    }
}


#[derive(Debug)]
pub struct ItemOptions {
    pub all: bool,
    pub dirs: bool,
    pub files: bool,
    pub other: bool,
}

pub fn items(dir_path: &Path, opt: &ItemOptions, excludes: &Excludes) -> Result<Vec<OsString>> {
    // eprintln!("items({dir_path:?}, {opt:?})");
    fs::read_dir(dir_path).with_context(
        || anyhow!("opening directory {dir_path:?} for reading"))?
    .filter_map(
        |entry_result: Result<fs::DirEntry, std::io::Error>| -> Option<Result<OsString>> {
            match entry_result {
                Ok(entry) => {
                    let ft = entry.file_type()
                        .expect("does this fail on OSes needing stat?");
                    let filename = entry.file_name();
                    if opt.all || ! generic_ignore_filename(&filename) {
                        let handle_as_dir = ft.is_dir()
                            && opt.dirs
                            && !excludes.dirs.contains(&filename);
                        let handle_as_file = ft.is_file()
                            && opt.files
                            && !excludes.files.contains(&filename);
                        let handle_as_other = opt.other &&
                            (!ft.is_dir() && !ft.is_file());
                        if handle_as_dir || (
                            handle_as_file || handle_as_other) {
                            Some(Ok(filename))
                        } else {
                            trace!(
                                "ignoring item '{:?}' (type {:?})",
                                filename, ft);
                            None
                        }
                    } else {
                        None
                    }
                },
                Err(e) =>
                    Some(Err(e).with_context(|| anyhow!("read_dir on {dir_path:?}")))
            }
        })
    .collect::<Result<_,_>>()
}
