//! More From implementations for Path

use std::{
    ffi::{CStr, CString, NulError, OsStr, OsString},
    os::unix::prelude::{OsStrExt, OsStringExt},
    path::{Path, PathBuf},
};

pub trait MoreTryFrom<T>: Sized {
    /// The type returned in the event of a conversion error.
    type Error;

    /// Performs the conversion.
    fn more_try_from(value: T) -> Result<Self, Self::Error>;
}

// Ah, for Unix only!
impl MoreTryFrom<&OsStr> for CString {
    type Error = NulError;

    fn more_try_from(value: &OsStr) -> Result<Self, Self::Error> {
        CString::new(value.to_owned().into_vec())
    }
}

// Ah, for Unix only!
impl MoreTryFrom<&OsString> for CString {
    type Error = NulError;

    fn more_try_from(value: &OsString) -> Result<Self, Self::Error> {
        CString::new(value.to_owned().into_vec())
    }
}

// Ah, for Unix only!
impl MoreTryFrom<OsString> for CString {
    type Error = NulError;

    fn more_try_from(value: OsString) -> Result<Self, Self::Error> {
        CString::new(value.into_vec())
    }
}

// Ah, for Unix only!
impl MoreTryFrom<&Path> for CString {
    type Error = NulError;

    fn more_try_from(value: &Path) -> Result<Self, Self::Error> {
        CString::more_try_from(OsString::from(value))
    }
}

// Ah, for Unix only!
impl MoreTryFrom<&PathBuf> for CString {
    type Error = NulError;

    fn more_try_from(value: &PathBuf) -> Result<Self, Self::Error> {
        CString::more_try_from(OsString::from(value))
    }
}

// Ah, for Unix only!
impl MoreTryFrom<PathBuf> for CString {
    type Error = NulError;

    fn more_try_from(value: PathBuf) -> Result<Self, Self::Error> {
        CString::more_try_from(OsString::from(value))
    }
}

pub trait MoreFrom<T>: Sized {
    fn more_from(value: T) -> Self;
}

// Ah, for Unix only, going via bytes assuming UTF-8!
impl<'t> MoreFrom<&'t CStr> for &'t OsStr {
    fn more_from(value: &'t CStr) -> Self {
        OsStr::from_bytes(value.to_bytes())
    }
}

// Ah, for Unix only, going via bytes assuming UTF-8!
impl<'t> MoreFrom<&'t CStr> for &'t Path {
    fn more_from(value: &'t CStr) -> Self {
        Path::new(OsStr::from_bytes(value.to_bytes()))
    }
}

// Ah, for Unix only, going via bytes assuming UTF-8!
impl MoreFrom<&CStr> for PathBuf {
    fn more_from(value: &CStr) -> Self {
        Path::new(OsStr::from_bytes(value.to_bytes())).into()
    }
}
