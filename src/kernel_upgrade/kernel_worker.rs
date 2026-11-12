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
    file_make::MakeError,
    gpg::Gpg,
    kernel_upgrade::{
        kernel::Kernel,
        mode::{Action, Mode},
    },
    script_tools::ScriptContext,
    scripting::linux::kernel_version::KernelVersion,
    util::anyhow::AnyhowError,
};
use external_webget::WebGet;

def_subdirs! {pub Dirs => kernels, hashes, patches, configs, stamps}

#[derive(Debug)]
pub struct KernelWorker {
    webget: WebGet,
    mode: Mode,
    script_context: ScriptContext,
    dirs: Dirs,
    _locked_file: File,
}

#[derive(Debug, thiserror::Error)]
pub enum KernelWorkerError {
    #[error("another kernel worker is running")]
    Locked,
    #[error(
        "no lock file exists yet that we can take the lock on -- \
         you are probably running as root and we don't want to \
         create a lock file that the non-root user cannot open for writing"
    )]
    NoLockFile,
    #[error("outside kernel worker: {0:#}")]
    Anyhow(#[from] anyhow::Error),
}

pub struct KernelWorkerOpts {
    /// Give a `KernelWorkerError::Locked` error if locked instead of
    /// blocking
    pub skip_if_locked: bool,
    /// Give a `KernelWorkerError::NoLockFile` error if lock file does
    /// not exist yet (useful if running as root and wanting to avoid
    /// creating that file as root and making it unwritable to
    /// non-root user)
    pub refuse_lock_file_creation: bool,
}

impl KernelWorker {
    /// When another instance is running, if `stop_if_locked` is true
    /// `Err(KernelWorkerError::Locked)` is returned, otherwise prints
    /// a message to stderr and waits for the lock to be release.
    pub fn new(
        base_dir: PathBuf,
        KernelWorkerOpts {
            skip_if_locked,
            refuse_lock_file_creation,
        }: KernelWorkerOpts,
        script_context: ScriptContext,
        mode: Mode,
    ) -> Result<Self, KernelWorkerError> {
        let lock_path = (&base_dir).append(".kernel-upgrade.flock");

        let dirs = Dirs::new(base_dir);
        dirs.create_dirs_if_not_exist()?;

        let _locked_file = if refuse_lock_file_creation {
            // flock works fine on read-only files, even with
            // `LOCK_EX`, except on NFS (man 2 flock). XX does anyone
            // care about using NFS for kernel builds?
            match File::open(&lock_path) {
                Ok(f) => f,
                Err(e) => match e.kind() {
                    std::io::ErrorKind::NotFound => {
                        Err(KernelWorkerError::NoLockFile)?
                    }
                    _ => Err(e).with_context(|| {
                        anyhow!("opening lock file {lock_path:?}")
                    })?,
                },
            }
        } else {
            File::create(&lock_path).with_context(|| {
                anyhow!("opening/creating lock file {lock_path:?}")
            })?
        };

        // File::lock in std is not in rustc from Debian stable yet,
        // thus use nix
        if flock(_locked_file.as_raw_fd(), FlockArg::LockExclusiveNonblock)
            .is_err()
        {
            if skip_if_locked {
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
            mode,
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
    pub fn mode(&self) -> Mode {
        self.mode
    }

    pub fn base_dir(&self) -> &Path {
        &self.dirs.base_dir
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
        self.mode.fm(Action::Retrieve).make(
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
