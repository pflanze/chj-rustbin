use crate::file_make::FileMake;

pub trait Fm {
    /// Give access to the `FileMake` context to use
    fn fm(&self) -> &FileMake;
}
