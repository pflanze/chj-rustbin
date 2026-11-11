use std::{fs::File, os::fd::AsRawFd, path::PathBuf};

use anyhow::{anyhow, Context, Result};
use cj_path_util::path_util::AppendToPath;
use nix::fcntl::{flock, FlockArg};

use crate::{
    def_subdirs,
    file_make::FileMake,
    kernel_upgrade::{fm::Fm, kernel::Kernel},
    scripting::linux::kernel_version::KernelVersion,
};
use external_webget::WebGet;

def_subdirs! {pub Dirs => kernels, hashes, patches, configs, stamps}

#[derive(Debug)]
pub struct KernelWorker {
    webget: WebGet,
    file_make: FileMake,
    dirs: Dirs,
    _locked_file: File,
}

impl Fm for KernelWorker {
    fn fm(&self) -> &FileMake {
        &self.file_make
    }
}

#[derive(Debug, thiserror::Error)]
pub enum KernelWorkerError {
    #[error("another kernel worker is running")]
    Locked,
    #[error("setting up kernel worker: {0:#}")]
    Anyhow(#[from] anyhow::Error),
}

impl KernelWorker {
    /// When another instance is running, if `stop_if_locked` is true
    /// `Err(KernelWorkerError::Locked)` is returned, otherwise prints
    /// a message to stderr and waits for the lock to be release.
    pub fn new(
        base_dir: PathBuf,
        stop_if_locked: bool,
    ) -> Result<Self, KernelWorkerError> {
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
            if stop_if_locked {
                Err(KernelWorkerError::Locked)?
            }
            eprintln!("waiting for lock file {lock_path:?} to be released");
            flock(_locked_file.as_raw_fd(), FlockArg::LockExclusive)
                .with_context(|| anyhow!("flock on lock file {lock_path:?}"))?;
        }

        Ok(Self {
            dirs,
            _locked_file,
            webget: WebGet::auto_choose()?,
            file_make: FileMake {},
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
