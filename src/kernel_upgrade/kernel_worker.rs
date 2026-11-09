use std::{fs::File, os::fd::AsRawFd, path::PathBuf};

use anyhow::{anyhow, Context, Result};
use cj_path_util::path_util::AppendToPath;
use nix::fcntl::{flock, FlockArg};

use crate::{
    def_subdirs, kernel_upgrade::kernel::Kernel,
    scripting::linux::kernel_version::KernelVersion,
};
use external_webget::WebGet;

def_subdirs! {pub Dirs => kernels, hashes, patches, configs, stamps}

#[derive(Debug)]
pub struct KernelWorker {
    webget: WebGet,
    dirs: Dirs,
    _locked_file: File,
}

impl KernelWorker {
    pub fn new(base_dir: PathBuf) -> Result<Self> {
        let lock_path = (&base_dir).append(".kernel-upgrade.flock");

        let dirs = Dirs::new(base_dir);
        dirs.create_dirs_if_not_exist()?;

        let _locked_file = File::create(&lock_path)
            .with_context(|| anyhow!("opening lock file {lock_path:?}"))?;

        // File::lock in std is not in rustc from Debian stable yet,
        // thus use nix
        if flock(_locked_file.as_raw_fd(), FlockArg::LockExclusiveNonblock)
            .is_err()
        {
            eprintln!("waiting for lock file {lock_path:?} to be released");
            flock(_locked_file.as_raw_fd(), FlockArg::LockExclusive)
                .with_context(|| anyhow!("flock on lock file {lock_path:?}"))?;
        }

        Ok(Self {
            dirs,
            _locked_file,
            webget: WebGet::auto_choose()?,
        })
    }

    pub fn webget(&self) -> &WebGet {
        &self.webget
    }
    pub fn dirs(&self) -> &Dirs {
        &self.dirs
    }

    /// Path to the linux source and build directory
    pub fn linux_path(&self) -> PathBuf {
        (&self.dirs.base_dir).append("linux")
    }

    pub fn current_kernel(&self) -> Result<Kernel<'_>> {
        Ok(Kernel {
            worker: self,
            version: KernelVersion::from_makefile_in_directory(
                &self.linux_path(),
            )?,
        })
    }

    pub fn kernel_with_version(&self, version: KernelVersion) -> Kernel<'_> {
        Kernel {
            worker: self,
            version,
        }
    }
}
