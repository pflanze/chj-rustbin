//! Similar to `trait GetTarAndUnpack`, but gets a patch file to
//! change the version of an already-unpacked dir to the next one
//!
//! `kernel.org` does not provide signatures for each patch file, but
//! a signed file with hashes, thus retrieve that and look up the hash
//! in it.

use std::{
    collections::HashMap,
    ffi::{OsStr, OsString},
    path::{Path, PathBuf},
    process::{Child, Command, Stdio},
    sync::Arc,
};

use anyhow::{anyhow, bail, Context, Result};
use bstr::{BStr, BString, ByteSlice};
use cj_path_util::{path_util::AppendToPath, unix::polyfill::add_extension};

use crate::{
    easy_webget::{EasyWebget, GetToFileError},
    file_make::MakeError,
    gpg::{Gpg, GpgKeyRef},
    kernel_upgrade::{
        fm::Fm, get_gpg_key::get_gpg_key, kernel::Kernel,
        kernel_worker::KernelWorker,
    },
    scripting::{linux::kernel_version::KernelVersion, sha256sum::sha256sum},
    util::{anyhow::AnyhowError, link_rename::link_rename},
};

/// Map a relative path to the SHA-256 hash hex string
pub struct PathFileHashes(pub(crate) HashMap<OsString, BString>);

#[derive(Debug, thiserror::Error)]
pub enum GetAndApplyError {
    #[error("{0:#}")]
    GetChecked(
        #[from] MakeError<MakeError<AnyhowError, GetToFileError>, AnyhowError>,
    ),
    #[error("{0:#}")]
    Anyhow(#[from] anyhow::Error),
}

/// Get patch hashes and verify the signature on them, as well as the
/// patch files themselves.
///
/// Currently only used for incremental patches
/// (`https://cdn.kernel.org/pub/linux/kernel/v*.x/incr/patch-*`), but
/// might also be used in the future for the
/// `https://cdn.kernel.org/pub/linux/kernel/v*.x/patch-*` files.
pub trait GetHashesAndPatch<'worker>: Fm {
    const HASHES_SIGNATURE_KEY: GpgKeyRef;

    fn worker(&self) -> &'worker KernelWorker;

    fn from_version(&self) -> &KernelVersion;

    /// Must create the directory if it doesn't exist
    fn hashes_base_dir(&self) -> Result<Arc<Path>>;

    /// Must create the directory if it doesn't exist
    fn patches_base_dir(&self) -> Result<Arc<Path>>;

    /// Should end in "/"
    fn hashes_and_patches_base_url(&self) -> String;

    // -----------------------------------------------------------------------------

    /// File name of the patch file (upstream and locally)
    fn file_name(&self) -> String {
        self.from_version().patch_filename_to_next()
    }

    fn hashes_asc_url(&self) -> String {
        self.hashes_and_patches_base_url() + "sha256sums.asc"
    }

    /// The unverified file
    fn get_hashes_asc(
        &self,
        newer_than_paths: &[&Path],
    ) -> Result<PathBuf, MakeError<AnyhowError, GetToFileError>> {
        self.fm().make(
            self.hashes_base_dir()
                .map_err(AnyhowError)?
                .append("sha256sums.asc"),
            newer_than_paths,
            |target, _| {
                self.worker()
                    .webget()
                    .get_to_file(&self.hashes_asc_url(), target)
            },
        )
    }

    fn get_gpg_key_touch_file(&self) -> Result<PathBuf> {
        Ok(self
            .hashes_base_dir()
            .map_err(AnyhowError)?
            .append(".get_gpg_key"))
    }

    /// Verifies the signature of the hashes list file via gpg
    fn get_hashes_file(
        &self,
        newer_than_paths: &[&Path],
    ) -> Result<PathBuf, MakeError<AnyhowError, AnyhowError>> {
        self.fm().make(
            self.hashes_base_dir()
                .map_err(AnyhowError)?
                .append("sha256sums"),
            [
                self.get_hashes_asc(newer_than_paths)
                    .map_err(anyhow::Error::from)
                    .map_err(AnyhowError)?,
                get_gpg_key(
                    self.fm(),
                    self.get_gpg_key_touch_file().map_err(AnyhowError)?,
                    &Self::HASHES_SIGNATURE_KEY.key_fingerprint,
                )
                .map_err(MakeError::into_make_error)?,
            ],
            |target, dependencies| -> Result<(), AnyhowError> {
                let [source, _keystamp] = dependencies;
                Gpg::default()
                    .verify_unpack(
                        source,
                        target,
                        Some(&Self::HASHES_SIGNATURE_KEY),
                    )
                    .with_context(|| {
                        anyhow!("verifying signature on {source:?}")
                    })?;
                Ok(())
            },
        )
    }

    /// Downloads, verifies and parses the sha256sum file.
    ///
    /// Give all the patch file paths you want to use the
    /// `PathFileHashes` on, so that the sha256sum is re-retrieved if
    /// any of them is newer! (Thankfully the hashes are only needed
    /// after getting a new patch file, so, file based caching is all
    /// that's needed to avoid overly many re-retrievals. Also, could
    /// just retry once missing a file in the hash set.)
    fn get_hashes(&self, newer_than_paths: &[&Path]) -> Result<PathFileHashes> {
        let hashes_path = self.get_hashes_file(newer_than_paths)?;

        eprintln!("parse {hashes_path:?}");
        let contents: BString = std::fs::read(&hashes_path)
            .with_context(|| anyhow!("reading {hashes_path:?}"))?
            .into();

        let mut lines: Vec<_> =
            contents.split(|c| *c == b'\n' || *c == b'\r').collect();
        if lines.last() == Some(&b"".as_slice()) {
            lines.pop();
        }

        let table = lines.iter()
            .enumerate()
            .map(|(i, line)|-> Result<(_, _)> {
                let line = line.as_bstr();
                let items : Vec<&BStr> = line.split_str(b"  ").map(|b| b.as_bstr()).collect();
                match items.as_slice() {
                    [hash, path] => {
                        // Uff. Only on Unix, and only from slices, *then* to_owned.
                        use std::os::unix::ffi::OsStrExt;
                        let path = OsStr::from_bytes(path).to_owned();
                        Ok((path, (*hash).to_owned()))
                    },
                    _ =>  bail!(
                        "file {hashes_path:?} line {} does not contain 2 items split by '  ': \
                         {items:?}",
                        i + 1
                    )
                }
            }).collect::<Result<HashMap<OsString, BString>>>()?;
        Ok(PathFileHashes(table))
    }

    fn file_path(&self) -> Result<PathBuf> {
        Ok(self.patches_base_dir()?.append(self.file_name()))
    }

    fn url(&self) -> String {
        self.hashes_and_patches_base_url() + &self.file_name()
    }

    /// Fetch or local file if present
    fn get_unchecked(
        &self,
    ) -> Result<PathBuf, MakeError<AnyhowError, GetToFileError>> {
        self.fm().make(
            add_extension(self.file_path().map_err(AnyhowError)?, "-unchecked")
                .expect("file_path is non-empty"),
            (),
            |target, ()| {
                self.worker().webget().get_to_file(&self.url(), target)
            },
        )
    }

    fn get_checked(
        &self,
    ) -> Result<
        PathBuf,
        MakeError<MakeError<AnyhowError, GetToFileError>, AnyhowError>,
    > {
        self.fm().make(
            self.file_path()
                .map_err(AnyhowError)
                .map_err(MakeError::Dependency)?,
            [self.get_unchecked()?],
            |target, dependencies| -> Result<(), AnyhowError> {
                let [source] = dependencies;

                let target_filename = target.file_name().ok_or_else(|| {
                    anyhow!("can't get file_name for {target:?}")
                })?;

                // First see if we have an existing file that might suit us
                let old_hashes = self.get_hashes(&[])?;
                let new_hashes;

                let sum_expected =
                    if let Some(sum) = old_hashes.0.get(target_filename) {
                        sum
                    } else {
                        // Have to actually get a sums file newer than the patch
                        new_hashes = self.get_hashes(&[source])?;
                        new_hashes.0.get(target_filename).ok_or_else(|| {
                            anyhow!("missing hash for file {target:?}")
                        })?
                    };

                let sum = sha256sum(source)?;

                if sum_expected == sum.as_bytes() {
                    eprintln!(
                        "hashes for {target:?} match: {sum_expected} == {sum}"
                    );

                    // Do not use rename--it would lead to re-fetching of
                    // the dependency! `link_rename` is fine as
                    // `webget.get_to_file` uses a tempfile-rename
                    // approach, hence never mutates the hard-linked file.
                    link_rename(source, target).with_context(|| {
                        anyhow!("linking {source:?} to {target:?}")
                    })?;
                    Ok(())
                } else {
                    Err(anyhow!(
                        "hashes do not match for {target:?}: \
                         expected {sum_expected}, got {sum}"
                    ))?
                }
            },
        )
    }

    /// Returns the new Kernel (in old place)
    // GetToFileError
    fn get_and_apply(&self) -> Result<Kernel<'worker>, GetAndApplyError> {
        let path = self.get_checked()?;

        let mut xzcat: Child = Command::new("xzcat")
            .arg(&*path)
            .stdout(Stdio::piped())
            .spawn()
            .context("starting 'xzcat'")?;

        let inp = xzcat.stdout.take().expect("requested above");

        let mut patch: Child = Command::new("patch")
            .arg("-p1")
            .current_dir(self.worker().linux_path())
            .stdin(inp)
            .spawn()
            .context("starting 'patch'")?;

        let mut errors = Vec::new();
        let xzcat_status = xzcat.wait().context("waiting for xzcat")?;
        if !xzcat_status.success() {
            errors.push(format!("xzcat failed: {xzcat_status}"));
        }
        let patch_status = patch.wait().context("waiting for patch")?;
        if !patch_status.success() {
            errors.push(format!("patch failed: {patch_status}"));
        }
        if !errors.is_empty() {
            Err(anyhow!(
                "applying {:?}: {}",
                self.file_path(),
                errors.join(", ")
            ))?;
        }

        Ok(Kernel {
            worker: self.worker(),
            version: self.from_version().sublevel_add(1),
        })
    }
}
