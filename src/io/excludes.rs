/// General basis for filesystem path exclusion logic.
use std::{
    collections::HashSet,
    ffi::{OsStr, OsString},
    os::unix::prelude::OsStrExt,
};

#[derive(Debug)]
pub struct Excludes {
    pub exclude_dot_files: bool,
    pub exclude_emacs_backups: bool,
    pub files: HashSet<OsString>,
    pub dirs: HashSet<OsString>,
}

impl Excludes {
    pub fn filename_is_excluded(
        &self,
        file_name: &OsStr,
        is_dir: bool,
    ) -> bool {
        (self.exclude_dot_files && filename_is_dot(&file_name))
            || (self.exclude_emacs_backups
                && filename_is_emacs_backup(&file_name))
            || (if is_dir { &self.dirs } else { &self.files })
                .contains(file_name)
    }
}

pub fn hashset_from(strs: &[&str]) -> HashSet<OsString> {
    let mut h: HashSet<OsString> = HashSet::new();
    for s in strs {
        h.insert(OsString::from(s));
    }
    h
}

/// If `all` is true, does not ignore exclude dot and emacs backup files.
pub fn default_excludes(all: bool) -> Excludes {
    Excludes {
        exclude_dot_files: !all,
        exclude_emacs_backups: !all,
        files: hashset_from(&["HEUTE", "CALENDAR"]),
        dirs: hashset_from(&[".git", ".METADATA-v2"]),
    }
}

/// If `all` is true, does not ignore exclude dot and emacs backup files.
pub fn empty_excludes(all: bool) -> Excludes {
    Excludes {
        exclude_dot_files: !all,
        exclude_emacs_backups: !all,
        files: HashSet::new(),
        dirs: HashSet::new(),
    }
}

pub fn filename_is_dot(filename: &OsStr) -> bool {
    let bs = filename.as_bytes();
    let len = bs.len();
    len >= 1 && (bs[0] == b'.')
}

pub fn filename_is_emacs_backup(filename: &OsStr) -> bool {
    let bs = filename.as_bytes();
    let len = bs.len();
    len >= 2 && (bs[len - 1] == b'~')
}

pub fn generic_ignore_filename(filename: &OsStr) -> bool {
    filename_is_dot(filename) || filename_is_emacs_backup(filename)
}
