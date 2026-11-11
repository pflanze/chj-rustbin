use std::{
    borrow::Cow,
    convert::Infallible,
    num::NonZeroUsize,
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::{anyhow, Context, Result};
use cj_path_util::path_util::AppendToPath;

use crate::{
    file_make::MakeError,
    gpg::{GpgKeyRef, KeyType},
    kernel_upgrade::{
        file_kind::{FileKind, FileName},
        fm::Fm,
        get_tar_and_unpack::GetTarAndUnpack,
        kernel_patch::KernelPatch,
        kernel_worker::KernelWorker,
    },
    scripting::linux::kernel_version::KernelVersion,
    util::anyhow::AnyhowError,
    utillib::into_arc_path::IntoArcPath,
};

const KERNEL_TAR_KEY: GpgKeyRef = GpgKeyRef {
    key_type: KeyType::RSA,
    key_fingerprint: Cow::Borrowed("647F28654894E3BD457199BE38DBBDC86092693E"),
};

#[derive(Debug)]
pub struct Kernel<'worker> {
    pub(crate) worker: &'worker KernelWorker,
    pub(crate) version: KernelVersion,
}

impl<'worker> Fm for Kernel<'worker> {
    fn fm(&self) -> &crate::file_make::FileMake {
        self.worker.fm()
    }
}

impl<'worker> Kernel<'worker> {
    pub fn worker(&self) -> &'worker KernelWorker {
        self.worker
    }
    pub fn version(&self) -> &KernelVersion {
        &self.version
    }

    /// Before appending the file name, or "incr/" subdir and file
    /// name parts for incremental patches (i.e. may not be
    /// retrievable directly)
    pub(crate) fn kernel_directory_url(&self) -> String {
        format!(
            "https://cdn.kernel.org/pub/linux/kernel/v{}.x/",
            self.version.version
        )
    }

    fn config_backup_path(&self) -> PathBuf {
        self.worker
            .dirs()
            .configs()
            .append(format!("config-{}", self.version.to_string()))
    }

    /// Returns the path to the backed up config file
    pub fn make_oldconfig(
        &self,
    ) -> Result<PathBuf, MakeError<Infallible, AnyhowError>> {
        let linux_path = &self.worker.linux_path();
        self.fm().make(
            self.config_backup_path(),
            [
                // Make sure there is an initial file, or `make oldconfig` will
                // generate a default config which is not going to be useful!
                linux_path.append(".config"),
                // (No need to depend on the mtime of anything else like the
                // Makefile, since the target is versioned and won't exist from
                // before updating to this kernel version.)
            ],
            |target, dependencies| {
                let ctx = self.worker().script_context();

                let [dependency] = dependencies;
                // (Should this region be redirected into a file? All of the
                // program's output, right, i.e. set stdout and stderr
                // temporarily?)
                ctx.set_cwd(linux_path).cmd("make", ["oldconfig"])?;
                std::fs::copy(dependency, target).with_context(|| {
                    anyhow!("copying {dependency:?} to {target:?}")
                })?;
                Ok(())
            },
        )
    }

    fn stamp_path(&self) -> PathBuf {
        self.worker.dirs().stamps().append(self.version.to_string())
    }

    /// (Returns the path to the stamp file)
    pub fn build(
        &self,
        num_cores: NonZeroUsize,
    ) -> Result<
        PathBuf,
        MakeError<MakeError<Infallible, AnyhowError>, AnyhowError>,
    > {
        let linux_path = self.worker.linux_path();
        self.fm().make(
            self.stamp_path(),
            [self.make_oldconfig()?],
            |target, _dependencies| {
                let ctx = self.worker().script_context();
                {
                    // (Should this region be redirected into a file? All of the
                    // program's output, right, i.e. set stdout and stderr
                    // temporarily?)

                    let ctx = ctx.set_cwd(&linux_path);

                    let j = &format!("-j{num_cores}");
                    ctx.cmd("make", [j])?;
                    ctx.cmd("make", [j, "modules"])?;
                }
                ctx.touch(target)?;
                Ok(())
            },
        )
    }

    pub fn the_patch_to_the_next_point_version(&self) -> KernelPatch<'worker> {
        KernelPatch {
            worker: self.worker,
            from_version: self.version.clone(),
        }
    }
}

impl<'worker> GetTarAndUnpack<'worker> for Kernel<'worker> {
    const HASHES_SIGNATURE_KEY: GpgKeyRef = KERNEL_TAR_KEY;

    fn worker(&self) -> &'worker KernelWorker {
        self.worker
    }

    fn download_dir(&self) -> Result<Arc<Path>> {
        Ok(self.worker().dirs().kernels().into_arc_path())
    }

    fn remote_base_url(&self) -> String {
        self.kernel_directory_url()
    }

    fn target_directory_path(&self) -> Result<PathBuf> {
        Ok(self.worker.linux_path())
    }

    fn file_name<T: FileKind>(&self) -> FileName<T> {
        format!("linux-{}.{}", self.version(), T::SUFFIX).into()
    }
}
