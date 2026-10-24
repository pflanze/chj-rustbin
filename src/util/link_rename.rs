use std::{
    fs::{hard_link, rename},
    path::Path,
};

use anyhow::anyhow;
use cj_path_util::unix::polyfill::add_extension;
use getrandom::getrandom;

#[derive(Debug, thiserror::Error)]
pub enum LinkRenameError {
    #[error("{0}")]
    Other(#[from] anyhow::Error),
    #[error("getting random number: {0}")]
    Getrandom(#[from] getrandom::Error),
    #[error("creating a hard link: {0}")]
    Link(std::io::Error),
    #[error("renaming the new hard link to the target location: {0}")]
    Rename(std::io::Error),
}

/// Create a new hard link from `from` to `to`, even if `to` exists
///
/// This is done by way of first making one to a temporary location
/// then renaming it.
pub fn link_rename(from: &Path, to: &Path) -> Result<(), LinkRenameError> {
    for _ in 0..30 {
        let mut rand = [0; 8];
        getrandom(&mut rand)?;
        let tmp =
            add_extension(from, format!(".tmp-{rand:?}")).ok_or_else(|| {
                anyhow!("could not add extension to path {from:?}")
            })?;
        match hard_link(from, &tmp) {
            Ok(()) => return rename(&tmp, to).map_err(LinkRenameError::Rename),
            Err(e) => match e.kind() {
                std::io::ErrorKind::AlreadyExists => (),
                _ => return Err(LinkRenameError::Link(e)),
            },
        }
    }
    Err(anyhow!(
        "ran out of attempts finding a non-conflicting temporary path"
    ))?
}
