/// General basis for filesystem path exclusion logic.

use std::{collections::HashSet, ffi::OsString, os::unix::prelude::OsStrExt};


#[derive(Debug)]
pub struct Excludes {
    pub files: HashSet<OsString>,
    pub dirs: HashSet<OsString>
}

pub fn hashset_from(strs: &[&str]) -> HashSet<OsString> {
    let mut h : HashSet<OsString> = HashSet::new();
    for s in strs {
        h.insert(OsString::from(s));
    }
    h
}

pub fn default_excludes() -> Excludes {
    Excludes {
        files: hashset_from(&["HEUTE", "CALENDAR"]),
        dirs: hashset_from(&[".git", ".METADATA-v2"])
    }
}

pub fn empty_excludes() -> Excludes {
    Excludes {
        files: HashSet::new(),
        dirs: HashSet::new(),
    }
}

pub fn generic_ignore_filename(filename: &OsString) -> bool {
    let bs = filename.as_bytes();
    let len = bs.len();
    len >= 1 && (bs[0] == b'.') ||
    len >= 2 && (bs[len-1] == b'~')
}

