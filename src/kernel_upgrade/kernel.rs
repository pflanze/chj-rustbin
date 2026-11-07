use std::{convert::Infallible, num::NonZeroUsize, path::PathBuf};

use anyhow::{anyhow, Context, Result};
use cj_path_util::path_util::AppendToPath;

use crate::{
    file_make::{file_make, MakeError},
    kernel_upgrade::{kernel_patch::KernelPatch, kernel_worker::KernelWorker},
    script_tools::{cmd, touch, CmdCwd},
    scripting::linux::kernel_version::KernelVersion,
    util::anyhow::AnyhowError,
};

#[derive(Debug)]
pub struct Kernel<'worker> {
    pub(crate) worker: &'worker KernelWorker,
    pub(crate) version: KernelVersion,
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
            .dirs
            .configs()
            .append(format!("config-{}", self.version.to_string()))
    }

    /// Returns the path to the backed up config file
    pub fn make_oldconfig(
        &self,
    ) -> Result<PathBuf, MakeError<Infallible, AnyhowError>> {
        let linux_path = self.worker.dirs.linux();
        file_make(
            self.config_backup_path(),
            [
                // Make sure there is an initial file, or `make oldconfig` will
                // generate a default config which is not going to be useful!
                self.worker.dirs.linux().append(".config"),
                // (No need to depend on the mtime of anything else like the
                // Makefile, since the target is versioned and won't exist from
                // before updating to this kernel version.)
            ],
            |target, dependencies| {
                let [dependency] = dependencies;
                {
                    // (Should this region be redirected into a file? All of the
                    // program's output, right, i.e. set stdout and stderr
                    // temporarily?)

                    let _cwd = CmdCwd::new(Some(linux_path.clone()));
                    cmd("make", ["oldconfig"])?;
                }
                std::fs::copy(dependency, target).with_context(|| {
                    anyhow!("copying {dependency:?} to {target:?}")
                })?;
                Ok(())
            },
        )
    }

    fn stamp_path(&self) -> PathBuf {
        self.worker.dirs.stamps().append(self.version.to_string())
    }

    /// (Returns the path to the stamp file)
    pub fn build(
        &self,
        num_cores: NonZeroUsize,
    ) -> Result<
        PathBuf,
        MakeError<MakeError<Infallible, AnyhowError>, AnyhowError>,
    > {
        let linux_path = self.worker.dirs.linux();
        file_make(
            self.stamp_path(),
            [self.make_oldconfig()?],
            |target, _dependencies| {
                {
                    // (Should this region be redirected into a file? All of the
                    // program's output, right, i.e. set stdout and stderr
                    // temporarily?)

                    let j = &format!("-j{num_cores}");
                    let _cwd = CmdCwd::new(Some(linux_path.clone()));
                    cmd("make", [j])?;
                    cmd("make", [j, "modules"])?;
                }
                touch(target)?;
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
