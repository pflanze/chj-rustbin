use std::{
    ffi::{OsStr, OsString},
    path::{Path, PathBuf},
    sync::Arc,
};

pub trait IntoArcPath {
    fn into_arc_path(self) -> Arc<Path>;
}

impl IntoArcPath for PathBuf {
    fn into_arc_path(self) -> Arc<Path> {
        self.into()
    }
}

impl IntoArcPath for &Path {
    fn into_arc_path(self) -> Arc<Path> {
        self.into()
    }
}

impl IntoArcPath for String {
    fn into_arc_path(self) -> Arc<Path> {
        PathBuf::from(self).into()
    }
}

impl IntoArcPath for &str {
    fn into_arc_path(self) -> Arc<Path> {
        Arc::<Path>::from(self.as_ref())
    }
}

impl IntoArcPath for OsString {
    fn into_arc_path(self) -> Arc<Path> {
        PathBuf::from(self).into()
    }
}

impl IntoArcPath for &OsStr {
    fn into_arc_path(self) -> Arc<Path> {
        Arc::<Path>::from(self.as_ref())
    }
}

// CString has no direct conversion to PathBuf, and this module should
// probably not offer one due to portability questions.
