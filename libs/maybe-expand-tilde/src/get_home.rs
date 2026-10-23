//! Trait for getting the home directory
//!
//! Currently only `current_user_home()`, but might add a
//! `user_home(user_name: &str)` in the future.

use std::{borrow::Cow, env::home_dir, path::Path};

pub trait GetHome<'h> {
    fn current_user_home(self) -> Option<Cow<'h, Path>>;
}

#[derive(Debug, Clone)]
pub struct FromOs;

impl<'h> GetHome<'h> for FromOs {
    fn current_user_home(self) -> Option<Cow<'static, Path>> {
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
