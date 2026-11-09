use std::path::PathBuf;

use anyhow::Result;
use cj_path_util::path_util::AppendToPath;

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
}

impl KernelWorker {
    pub fn new(base_dir: PathBuf) -> Result<Self> {
        let dirs = Dirs::new(base_dir);
        dirs.create_dirs_if_not_exist()?;

        Ok(Self {
            dirs,
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
