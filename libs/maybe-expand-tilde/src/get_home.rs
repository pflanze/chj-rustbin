//! Trait for getting the home directory
//!
//! Currently only `current_user_home()`, but might add a
//! `user_home(user_name: &str)` in the future.

#[allow(deprecated)]
use std::{borrow::Cow, env::home_dir, path::Path};

pub trait GetHome<'h> {
    fn current_user_home(self) -> Option<Cow<'h, Path>>;
}

#[derive(Debug, Clone)]
pub struct FromOs;

impl<'h> GetHome<'h> for FromOs {
    fn current_user_home(self) -> Option<Cow<'static, Path>> {
        // Source says "This function's behavior may be unexpected on
        // Windows. Consider using a crate from crates.io instead."; but, so far
        // this code has no Windows users(?), and which crate, and why would
        // they do better than this documents? Also, "Before Rust 1.85.0, this
        // function used to return the value of the 'HOME' environment variable
        // on Windows", so I guess that is the confusing bit, but, I don't use
        // older compilers than Debian stable's, so is it fine again?
        #[allow(deprecated)]
        home_dir().map(Into::into)
    }
}

#[derive(Debug, Clone)]
pub struct TheHome<'h>(pub Option<Cow<'h, Path>>);

impl<'h> GetHome<'h> for TheHome<'h> {
    fn current_user_home(self) -> Option<Cow<'h, Path>> {
        self.0
    }
}
