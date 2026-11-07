//! An explicit `AsRef<OsStr>`
//!
//! This is useful when the context is type-generic, to avoid having
//! to specify OsStr manually.

use std::ffi::OsStr;
pub trait AsOsStr {
    fn as_os_str(&self) -> &OsStr;
}

impl<T: AsRef<OsStr>> AsOsStr for T {
    fn as_os_str(&self) -> &OsStr {
        self.as_ref()
    }
}
