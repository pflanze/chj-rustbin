use std::{convert::Infallible, path::Path};

use anyhow::Result;

use crate::{
    file_make::{file_make, MakeError},
    gpg::Gpg,
    script_tools::touch,
    util::anyhow::AnyhowError,
};

/// Get gpg key if necessary
pub(crate) fn get_gpg_key<P: AsRef<Path>>(
    touch_file: P,
    key_fingerprint: &str,
) -> Result<P, MakeError<Infallible, AnyhowError>> {
    file_make(
        touch_file,
        (),
        |target, _dependencies| -> Result<(), AnyhowError> {
            Gpg::default().recv_key(key_fingerprint)?;
            touch(target.as_ref())?;
            Ok(())
        },
    )
}
