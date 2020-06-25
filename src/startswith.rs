use std::ffi::{OsStr, OsString};
use std::os::unix::ffi::OsStrExt;

pub trait StartsWith<T> {
    fn starts_with(&self, start: T) -> Option<usize>;
}

impl StartsWith<&OsString> for OsStr {
    fn starts_with(&self, start: &OsString) -> Option<usize> {
        let mut ia = self.as_bytes().iter();
        let mut ib = start.as_bytes().iter();
        let mut len: usize = 0;
        loop {
            if let Some(b) = ib.next() {
                if let Some(a) = ia.next() {
                    if a != b {
                        return None;
                    } else {
                        len += 1;
                    }
                } else {
                    return None;
                }
            } else {
                return Some(len);
            }
        }
    }
}
