//! Expand the `~` at the beginning of unix style path strings to the
//! actual home directory
//!
//! The `expand_tilde_string` function unconditionally expands such
//! paths, whereas the `MaybeExpandTilde` trait only does it for
//! string, but not path types. This aims to make it easy to write
//! functions that can take either input but only expand tilde paths
//! if it is safe to do so (given some assumptions).

pub mod get_home;

use std::{
    borrow::Cow,
    path::{Path, PathBuf},
};

use get_home::GetHome;
use into_cow_path::IntoCowPath;

/// Unconditionally expand the `~` at the start of `path`, except if
/// `get_home.current_user_home()` returns `None`, in which case the
/// original path is retained
pub fn expand_tilde_string<'t, S, H: GetHome<'t>>(
    path: S,
    get_home: H,
) -> Cow<'t, Path>
where
    S: IntoCowPath<'t>,
{
    let p = path.as_path();
    if p == Path::new("~") {
        if let Some(home) = get_home.current_user_home() {
            return home.into();
        }
    } else if let Ok(rest) = p.strip_prefix("~/") {
        if let Some(home) = get_home.current_user_home() {
            let mut home = home.into_owned();
            home.push(rest);
            return home.into();
        }
    }
    path.into_cow_path()
}

#[test]
fn t_expand_tilde_string() {
    use std::ffi::OsString;

    use crate::get_home::TheHome;

    let t = |s: &str| -> String {
        let path: PathBuf = expand_tilde_string(
            s,
            TheHome(Some(Cow::Borrowed("HOME".as_ref()))),
        )
        .into_owned();
        let s: OsString = path.into_os_string();
        s.into_string().unwrap()
    };

    assert_eq!(t("foo"), "foo");
    assert_eq!(t(""), "");
    assert_eq!(t("~foo"), "~foo");
    assert_eq!(t("~"), "HOME");
    assert_eq!(t("~/"), "HOME");
    assert_eq!(t(" ~/"), " ~/");
    assert_eq!(t("~ /"), "~ /");
    assert_eq!(t("/~/"), "/~/");
    assert_eq!(t("~/foo"), "HOME/foo");
    assert_eq!(t("~//foo"), "HOME/foo");
    assert_eq!(t("~///foo"), "HOME/foo");
    assert_eq!(t("~/././foo//bar"), "HOME/foo//bar");
    assert_eq!(t("~\\foo"), "~\\foo");
    assert_eq!(t("~/\0foo"), "HOME/\0foo");
    assert_eq!(t("~\0/foo"), "~\0/foo");
    assert_eq!(t("\0~/foo"), "\0~/foo");
}

/// Possibly expand the `~` at the beginning of `self`. This trait is
/// implemented as a no-op (i.e.leaving the input unchanged) for
/// `Path`-related types (under the assumption that those paths are
/// never provided via human input, but come from e.g. reading a
/// directory), but to call `expand_tilde_string` for string-related
/// types (under the assumption that string types are only provided
/// via human input, thus no paths that can be tricked into starting
/// with the tilde).
///
/// By default, owned strings and non-static string references define
/// this trait (with tilde expansion). If the
/// `only_static_path_strings` feature is declared, only static string
/// references define this trait (with tilde expansion), for other
/// string types the trait remains undefined. This restriction could
/// make the distinction between converting and non-converting types
/// safer.
pub trait MaybeExpandTilde<'t> {
    fn maybe_expand_tilde(self, get_home: impl GetHome<'t>) -> Cow<'t, Path>;
}

#[cfg(feature = "only_static_path_strings")]
impl<'h> MaybeExpandTilde<'h> for &'static str {
    fn maybe_expand_tilde(self, get_home: impl GetHome<'h>) -> Cow<'h, Path> {
        expand_tilde_string(self, get_home)
    }
}

#[cfg(not(feature = "only_static_path_strings"))]
impl<'h> MaybeExpandTilde<'h> for &'h str {
    fn maybe_expand_tilde(self, get_home: impl GetHome<'h>) -> Cow<'h, Path> {
        expand_tilde_string(self, get_home)
    }
}

#[cfg(feature = "only_static_path_strings")]
impl<'h> MaybeExpandTilde<'h> for &'static String {
    fn maybe_expand_tilde(self, get_home: impl GetHome<'h>) -> Cow<'h, Path> {
        expand_tilde_string(self, get_home)
    }
}

#[cfg(not(feature = "only_static_path_strings"))]
impl<'h> MaybeExpandTilde<'h> for &'h String {
    fn maybe_expand_tilde(self, get_home: impl GetHome<'h>) -> Cow<'h, Path> {
        expand_tilde_string(self, get_home)
    }
}

#[cfg(not(feature = "only_static_path_strings"))]
impl<'h> MaybeExpandTilde<'h> for String {
    fn maybe_expand_tilde(self, get_home: impl GetHome<'h>) -> Cow<'h, Path> {
        expand_tilde_string(self, get_home)
    }
}

impl<'h> MaybeExpandTilde<'h> for &'h Path {
    fn maybe_expand_tilde(self, _get_home: impl GetHome<'h>) -> Cow<'h, Path> {
        self.into()
    }
}

impl<'h> MaybeExpandTilde<'h> for &'h PathBuf {
    fn maybe_expand_tilde(self, _get_home: impl GetHome<'h>) -> Cow<'h, Path> {
        self.into()
    }
}

impl<'h> MaybeExpandTilde<'h> for PathBuf {
    fn maybe_expand_tilde(self, _get_home: impl GetHome<'h>) -> Cow<'h, Path> {
        self.into()
    }
}

/// Convenience to shadow a variable with one of the same name with
/// the path maybe expanded.
#[macro_export]
macro_rules! let_path_expand_tilde {
    { $var:ident } => {
        let $var = {
            use ::maybe_expand_tilde::{get_home::FromOs, MaybeExpandTilde};
            $var.maybe_expand_tilde(FromOs)
        };
    }
}
