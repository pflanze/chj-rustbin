//! Running the GnuPG gpg tool as external process invocations

use std::path::Path;

use anyhow::{anyhow, Context};
use bstr::BString;
use cj_path_util::temp_path::{TempPathOptions, TempfileError};

use crate::script_tools::cmd_capturing_stderr;

#[derive(Debug, Default)]
pub struct Gpg {}

#[derive(Debug, thiserror::Error)]
pub enum VerifyError {
    #[error("{0}")]
    Anyhow(#[from] anyhow::Error),
    #[error("{0}")]
    TempfileError(#[from] TempfileError),
}

impl Gpg {
    /// Unpack a signed file
    ///
    /// Uses a temp file and rename, only renames to `unpacked_file`
    /// once gpg exited successfully, i.e. the signature is verified.
    ///
    /// Returns the captured stderr of gpg; you may want to check it
    /// for warnings.
    pub fn verify_unpack(
        &self,
        signed_file: &Path,
        unpacked_file: &Path,
    ) -> Result<BString, VerifyError> {
        let unpacked_temp_path =
            TempPathOptions::default().temp_path_for(unpacked_file)?;
        let args = &[
            "-o".as_ref(),
            unpacked_temp_path.temp_path.as_os_str(),
            "--verify".as_ref(),
            signed_file.as_os_str(),
        ];
        let output = cmd_capturing_stderr("gpg", args, &[])
            .with_context(|| anyhow!("executing 'gpg' {args:?}"))?;
        unpacked_temp_path.persist()?;
        Ok(output)
    }
}
