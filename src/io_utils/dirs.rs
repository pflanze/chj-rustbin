use std::{fs::create_dir, path::Path};

pub fn create_dir_if_not_exists(path: &Path) -> Result<(), std::io::Error> {
    match create_dir(path) {
        Ok(()) => Ok(()),
        Err(e) => match e.kind() {
            std::io::ErrorKind::AlreadyExists => Ok(()),
            _ => Err(e),
        },
    }
}
