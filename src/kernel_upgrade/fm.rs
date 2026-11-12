use crate::{file_make::FileMake, kernel_upgrade::mode::Action};

pub trait Fm {
    /// Give access to the `FileMake` context to use
    fn fm(&self, for_action: Action) -> FileMake;
}
