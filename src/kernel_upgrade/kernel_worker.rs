use std::{
    convert::Infallible,
    fs::File,
    os::fd::AsRawFd,
    path::{Path, PathBuf},
};

use anyhow::{anyhow, Context, Result};
use cj_path_util::path_util::AppendToPath;
use nix::fcntl::{flock, FlockArg};

use crate::{
    def_subdirs,
    file_make::{FileMake, MakeError},
    gpg::Gpg,
    kernel_upgrade::{fm::Fm, kernel::Kernel},
    script_tools::ScriptContext,
    scripting::linux::kernel_version::KernelVersion,
    util::anyhow::AnyhowError,
};
use external_webget::WebGet;

def_subdirs! {pub Dirs => kernels, hashes, patches, configs, stamps}

#[derive(Debug)]
pub struct KernelWorker {
    webget: WebGet,
    file_make: FileMake,
    script_context: ScriptContext,
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
        script_context: ScriptContext,
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
            script_context,
        })
    }

    pub fn webget(&self) -> &WebGet {
        &self.webget
    }
    pub fn dirs(&self) -> &Dirs {
        &self.dirs
    }
    pub fn script_context(&self) -> &ScriptContext {
        &self.script_context
    }

    /// Path to the linux source and build directory
    pub fn linux_path(&self) -> PathBuf {
        (&self.dirs.base_dir).append("linux")
    }

    pub fn current_kernel(&self) -> Result<Kernel<'_>> {
        Ok(Kernel {
            worker: self,
            version: KernelVersion::from_makefile_in_directory(
                &self.script_context,
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

    /// Get gpg key if necessary
    ///
    /// `touch_file` is relative to `self.script_context.cwd`!
    pub(crate) fn get_gpg_key<P: AsRef<Path>>(
        &self,
        touch_file: P,
        key_fingerprint: &str,
    ) -> Result<P, MakeError<Infallible, AnyhowError>> {
        self.fm().make(
            touch_file,
            (),
            |target, _dependencies| -> Result<(), AnyhowError> {
                Gpg::default().recv_key(key_fingerprint)?;
                self.script_context.touch(target.as_ref())?;
                Ok(())
            },
        )
    }
}
