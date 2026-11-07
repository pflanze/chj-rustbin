//! Running the GnuPG gpg tool as external process invocations

use std::{borrow::Cow, fmt::Display, path::Path};

use anyhow::{anyhow, Context, Result};
use bstr::{BString, ByteSlice};
use cj_path_util::temp_path::{TempPathOptions, TempfileError};

use crate::script_tools::cmd_capturing_stderr;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KeyType {
    RSA,
}

impl AsRef<str> for KeyType {
    fn as_ref(&self) -> &str {
        match self {
            KeyType::RSA => "RSA",
        }
    }
}

impl Display for KeyType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_ref())
    }
}

pub struct GpgKeyRef {
    pub key_type: KeyType,
    /// The full fingerprint, without spaces!
    pub key_fingerprint: Cow<'static, str>,
}

impl GpgKeyRef {
    fn expected_message(&self) -> String {
        let Self {
            key_type,
            key_fingerprint,
        } = self;
        format!("using {key_type} key {key_fingerprint}")
    }
}

#[derive(Debug, Default)]
pub struct Gpg {}

#[derive(Debug, thiserror::Error)]
pub enum VerifyError {
    #[error("{0:#}")]
    Anyhow(#[from] anyhow::Error),
    #[error("{0:#}")]
    TempfileError(#[from] TempfileError),
    #[error(
        "missing the message {expected_message:?} in GPG output:\n{output}"
    )]
    WrongKey {
        expected_message: String,
        output: BString,
    },
}

impl Gpg {
    /// Run --recv-key for the given fingerprint, specifying
    /// "keyserver.ubuntu.com" as the keyserver
    pub fn recv_key(&self, fingerprint: &str) -> Result<BString> {
        cmd_capturing_stderr(
            "gpg",
            &[
                "--keyserver",
                "keyserver.ubuntu.com",
                "--recv-key",
                fingerprint,
            ],
            &[],
        )
    }

    /// Verify a separate signature on a file
    ///
    /// If `restrict_to_key` is given, verifies that the key was used
    /// for signing (the check is a bit simplicistic, but should never
    /// accept a signature that does *not* satisfy the key).
    ///
    /// Does not generate any files, just return a Result, which is Ok
    /// only if the files are present, the key for the signature is on
    /// the key chain, the signature is correct and matches the
    /// `signed_file`, and if given, `restrict_to_key` matches.
    ///
    /// Returns the captured stderr of gpg.
    pub fn verify(
        &self,
        signature_file: &Path,
        signed_file: &Path,
        restrict_to_key: Option<&GpgKeyRef>,
    ) -> Result<BString, VerifyError> {
        let args = &[
            "--verify".as_ref(),
            signature_file.as_os_str(),
            signed_file.as_os_str(),
        ];
        let output = cmd_capturing_stderr("gpg", args, &[])
            .with_context(|| anyhow!("executing 'gpg' {args:?}"))?;

        if let Some(key) = restrict_to_key {
            let expected_message = key.expected_message();
            if !output.contains_str(&expected_message) {
                return Err(VerifyError::WrongKey {
                    expected_message,
                    output,
                });
            }
        }

        Ok(output)
    }

    /// Unpack a signed file
    ///
    /// Uses a temp file and rename, only renames to `unpacked_file`
    /// once gpg exited successfully, i.e. the signature is verified.
    ///
    /// If `restrict_to_key` is given, verifies that the key was used
    /// for signing (the check is a bit simplicistic, but should never
    /// accept a signature that does *not* satisfy the key).
    ///
    /// Returns the captured stderr of gpg.
    pub fn verify_unpack(
        &self,
        signed_file: &Path,
        unpacked_file: &Path,
        restrict_to_key: Option<&GpgKeyRef>,
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

        if let Some(key) = restrict_to_key {
            let expected_message = key.expected_message();
            if !output.contains_str(&expected_message) {
                return Err(VerifyError::WrongKey {
                    expected_message,
                    output,
                });
            }
        }

        unpacked_temp_path.persist()?;

        Ok(output)
    }
}
