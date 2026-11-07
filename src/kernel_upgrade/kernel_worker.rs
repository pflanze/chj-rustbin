use anyhow::Result;

use crate::{
    def_subdirs, kernel_upgrade::kernel::Kernel,
    scripting::linux::kernel_version::KernelVersion,
};
use external_webget::WebGet;

def_subdirs! {pub Dirs => linux, hashes, patches, configs, stamps}

#[derive(Debug)]
pub struct KernelWorker {
    pub webget: WebGet,
    pub dirs: Dirs,
}

impl KernelWorker {
    pub fn current_kernel(&self) -> Result<Kernel<'_>> {
        Ok(Kernel {
            worker: self,
            version: KernelVersion::from_makefile_in_directory(
                &self.dirs.linux(),
            )?,
        })
    }
}
