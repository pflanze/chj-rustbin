//! Similar to `trait GetHashesAndPatch` but gets tar snapshot files,
//! not patches, and unpacks them
//!
//! Unlike for patch files, `kernel.org` offers a signature file on
//! each tar file individually, although the compressed tar file has
//! to be decompressed for the signature check.
//!
//! (While "get tar, verify then unpack" could be generic, there are
//! some linux-specific assumptions here.)

use std::{
    fs::{remove_file, rename, File},
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::{anyhow, Context, Result};
use cj_path_util::{
    path_util::AppendToPath, temp_file::unbuffered_temp_file_for,
};

use crate::{
    easy_webget::{EasyWebget, GetToFileError},
    file_make::{file_make, MakeError},
    gpg::{Gpg, GpgKeyRef},
    kernel_upgrade::{
        file_kind::{
            FileKind, FileKindSignature, FileKindTarChecked,
            FileKindTarUnchecked, FileKindTarXzUnchecked, FileName, FilePath,
        },
        kernel_worker::KernelWorker,
    },
    script_tools::{cmd_with_stdin_from, cmd_with_stdout_to, ls_paths, CmdCwd},
    util::{anyhow::AnyhowError, as_os_str::AsOsStr},
    utillib::into_arc_path::IntoArcPath,
};

pub trait GetTarAndUnpack<'worker> {
    const HASHES_SIGNATURE_KEY: GpgKeyRef;

    fn worker(&self) -> &'worker KernelWorker;

    /// Must create the directory if it doesn't exist
    fn download_dir(&self) -> Result<Arc<Path>>;

    /// Should end in "/"
    fn remote_base_url(&self) -> String;

    /// Must create the *parent* directory if it doesn't exist
    fn target_directory_path(&self) -> Result<PathBuf>;

    fn file_name<T: FileKind>(&self) -> FileName<T>;

    // -----------------------------------------------------------------------------

    fn file_path<T: FileKind>(&self) -> Result<FilePath<T>> {
        Ok(self.download_dir()?.append(&*self.file_name::<T>()).into())
    }

    fn url<T: FileKind>(&self) -> String {
        self.remote_base_url() + &self.file_name::<T>()
    }

    /// Fetch or local file if present
    fn get_signature(
        &self,
    ) -> Result<
        FilePath<FileKindSignature>,
        MakeError<AnyhowError, GetToFileError>,
    > {
        file_make(
            self.file_path::<FileKindSignature>()
                .map_err(|e| MakeError::Dependency(AnyhowError(e)))?,
            (),
            |target, ()| {
                self.worker()
                    .webget()
                    .get_to_file(&self.url::<FileKindSignature>(), target)
            },
        )
    }

    /// Fetch or local file if present
    fn get_compressed(
        &self,
    ) -> Result<
        FilePath<FileKindTarXzUnchecked>,
        MakeError<AnyhowError, GetToFileError>,
    > {
        file_make(
            self.file_path::<FileKindTarXzUnchecked>()
                .map_err(AnyhowError)?,
            (),
            |target, ()| {
                self.worker()
                    .webget()
                    .get_to_file(&self.url::<FileKindTarXzUnchecked>(), target)
            },
        )
    }

    /// Decompress or local file if present
    fn get_decompressed_unchecked(
        &self,
    ) -> Result<
        FilePath<FileKindTarUnchecked>,
        MakeError<MakeError<AnyhowError, GetToFileError>, AnyhowError>,
    > {
        file_make(
            self.file_path::<FileKindTarUnchecked>()
                .map_err(|e| MakeError::Dependency(AnyhowError(e)))?,
            [self.get_compressed()?],
            |target, dependencies| -> Result<(), AnyhowError> {
                let [compressed_path] = dependencies;
                let (tmp_path, tmp_file) =
                    unbuffered_temp_file_for(target.as_ref(), None)
                        .context("creating file for make target")?
                        .into_constituents();
                cmd_with_stdout_to(
                    "xz",
                    [
                        "-d".as_os_str(),
                        "--stdout".as_os_str(),
                        compressed_path.as_path().as_ref(),
                    ],
                    tmp_file,
                    &[],
                )?;
                tmp_path.persist().context("renaming file to make target")?;
                Ok(())
            },
        )
    }

    fn get_decompressed_checked(
        &self,
    ) -> Result<
        FilePath<FileKindTarChecked>,
        MakeError<MakeError<AnyhowError, GetToFileError>, AnyhowError>,
    > {
        file_make(
            self.file_path::<FileKindTarChecked>().map_err(|e| {
                MakeError::Dependency(MakeError::Dependency(AnyhowError(e)))
            })?,
            [self.get_signature()?.as_ref()],
            |target, dependencies| -> Result<(), AnyhowError> {
                let [signature_path] = dependencies;

                // Not making this a build dependency since once
                // `target` exists it doesn't matter whether this is
                // newer.
                let unchecked_path = self
                    .get_decompressed_unchecked()
                    .context("during check build action")?;

                Gpg::default().verify(
                    &signature_path,
                    &unchecked_path,
                    Some(&Self::HASHES_SIGNATURE_KEY)
                )
                    .with_context(|| {
                        anyhow!("verifying signature {signature_path:?} on {unchecked_path:?}")
                    })?;

                rename(&unchecked_path, target).with_context(|| {
                    anyhow!("renaming {unchecked_path:?} to {target:?}")
                })?;

                Ok(())
            },
        )
    }

    /// Get the path to the unpacked kernel directory,
    /// downloading/verifying/unpacking a snapshot if missing
    fn get_unpacked(
        &self,
    ) -> Result<PathBuf, MakeError<AnyhowError, AnyhowError>> {
        file_make(
            self.target_directory_path().map_err(AnyhowError)?,
            (),
            |target, ()| -> Result<(), AnyhowError> {
                let tar_path = self
                    .get_decompressed_checked()
                    .context("about to unpack")?;
                let parent = target.parent().ok_or_else(|| {
                    anyhow!("can't take parent dir of {target:?}")
                })?;
                let tmpdir =
                    tempfile::tempdir_in(&parent).with_context(|| {
                        anyhow!("creating temporary dir at {parent:?}")
                    })?;

                // Open the file before doing the chdir for running tar
                let input = File::open(&tar_path).with_context(|| {
                    anyhow!("opening tar file {tar_path:?} for input")
                })?;
                {
                    let _dir = CmdCwd::new(Some(tmpdir.path().into_arc_path()));
                    cmd_with_stdin_from(input, "tar", ["x"], &[])?;
                }
                let paths = ls_paths(tmpdir.path())?;
                match paths.as_slice() {
                    [linux_dir] => {
                        rename(linux_dir, &target)
                            .with_context(
                                || anyhow!(
                                    "renaming dir from {linux_dir:?} to {target:?}"
                                ))?;

                        // Don't need the unpacked tar anymore:
                        _= remove_file(&tar_path);

                        Ok(())
                    }
                    _ => Err(anyhow!(
                        "expecting 1 unpacked entry from the tar file, got: {paths:?}"
                    ))?
                }
            },
        )
    }
}
