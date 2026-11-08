//! Yet another file path and string type tagger
//!
//! This one knows about suffixes, though (specific for kernel.org
//! website).

use std::{
    marker::PhantomData,
    path::{Path, PathBuf},
};

use derive_more::Deref;

pub trait FileKind {
    const SUFFIX: &str;
}

macro_rules! def_file_kind {
    { { $(#[doc = $doc:literal])* } $name:ident, $suffix:expr } => {
        $(#[doc = $doc])*
            #[derive(Debug, Clone, Copy)]
        pub struct $name;
        impl FileKind for $name {
            const SUFFIX: &str = $suffix;
        }
    }
}

def_file_kind! {
    {
        /// File name of the file (upstream and locally)
    }
    FileKindTarXzUnchecked,
    "tar.xz"
}

def_file_kind! {
    {
        /// Used temporarily locally only, for the unchecked file.
    }
    FileKindTarUnchecked,
    "tar.-unchecked"
}

def_file_kind! {
    {
        /// Used locally only, for the checked file. Deleted again after
        /// unpacking.
    }
    FileKindTarChecked,
    "tar"
}

def_file_kind! {
    {
        /// File name of the file (upstream and locally)
    }
    FileKindSignature,
    "tar.sign"
}

#[derive(Debug, Deref)]
pub struct FileName<T: FileKind>(PhantomData<T>, #[deref] String);

impl<T: FileKind> From<String> for FileName<T> {
    fn from(value: String) -> Self {
        Self(PhantomData, value)
    }
}

impl<T: FileKind> AsRef<str> for FileName<T> {
    fn as_ref(&self) -> &str {
        &self.1
    }
}

#[derive(Debug, Deref)]
pub struct FilePath<T: FileKind>(PhantomData<T>, #[deref] PathBuf);

impl<T: FileKind> From<PathBuf> for FilePath<T> {
    fn from(value: PathBuf) -> Self {
        Self(PhantomData, value)
    }
}

impl<T: FileKind> AsRef<Path> for FilePath<T> {
    fn as_ref(&self) -> &Path {
        &self.1
    }
}
