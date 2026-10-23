//! Efficiently convert a string or path type into a `Cow<'_, Path>`
//! or `&Path` reference

use std::{
    borrow::Cow,
    path::{Path, PathBuf},
};

pub trait IntoCowPath<'s> {
    fn as_path(&self) -> &Path;
    fn into_cow_path(self) -> Cow<'s, Path>;
}

impl<'s, S: ?Sized> IntoCowPath<'s> for &'s S
where
    S: AsRef<Path>,
{
    #[inline]
    fn as_path(&self) -> &Path {
        (*self).as_ref()
    }

    #[inline]
    fn into_cow_path(self) -> Cow<'s, Path> {
        // Can't use as_path() since the returned lifetime is too
        // short
        let p: &Path = self.as_ref();
        p.into()
    }
}

// Why can't we have IsOwned so that we could impl <'s, S: IsOwned + ...> ?
impl<'s> IntoCowPath<'s> for String {
    #[inline]
    fn as_path(&self) -> &Path {
        self.as_ref()
    }

    #[inline]
    fn into_cow_path(self) -> Cow<'s, Path> {
        let p: PathBuf = self.into();
        p.into()
    }
}

impl<'s> IntoCowPath<'s> for Box<str> {
    #[inline]
    fn as_path(&self) -> &Path {
        (**self).as_ref()
    }

    #[inline]
    fn into_cow_path(self) -> Cow<'s, Path> {
        let s: String = self.into();
        let p: PathBuf = s.into();
        p.into()
    }
}

impl<'s> IntoCowPath<'s> for Cow<'s, str> {
    #[inline]
    fn as_path(&self) -> &Path {
        (**self).as_ref()
    }

    #[inline]
    fn into_cow_path(self) -> Cow<'s, Path> {
        match self {
            Cow::Borrowed(r) => Cow::Borrowed(r.as_ref()),
            Cow::Owned(s) => Cow::Owned(s.into()),
        }
    }
}

impl<'s> IntoCowPath<'s> for PathBuf {
    #[inline]
    fn as_path(&self) -> &Path {
        self
    }

    #[inline]
    fn into_cow_path(self) -> Cow<'s, Path> {
        self.into()
    }
}

impl<'s> IntoCowPath<'s> for Box<Path> {
    #[inline]
    fn as_path(&self) -> &Path {
        self
    }

    #[inline]
    fn into_cow_path(self) -> Cow<'s, Path> {
        let p: PathBuf = self.into();
        p.into()
    }
}

impl<'s> IntoCowPath<'s> for Cow<'s, Path> {
    #[inline]
    fn as_path(&self) -> &Path {
        self
    }

    #[inline]
    fn into_cow_path(self) -> Cow<'s, Path> {
        self
    }
}
