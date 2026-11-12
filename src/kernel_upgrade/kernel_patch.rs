use std::{borrow::Cow, path::Path, sync::Arc};

use anyhow::Result;
use cj_path_util::path_util::AppendToPath;

use crate::{
    file_make::FileMake,
    gpg::{GpgKeyRef, KeyType},
    io_utils::dirs::create_dir_if_not_exists,
    kernel_upgrade::{
        fm::Fm, get_hashes_and_patch::GetHashesAndPatch, kernel::Kernel,
        kernel_worker::KernelWorker, mode::Action,
    },
    scripting::linux::kernel_version::KernelVersion,
};

const KERNEL_INCR_KEY: GpgKeyRef = GpgKeyRef {
    key_type: KeyType::RSA,
    key_fingerprint: Cow::Borrowed("B8868C80BA62A1FFFAF5FDA9632D3A06589DA6B1"),
};

#[derive(Debug)]
pub struct KernelPatch<'worker> {
    pub(crate) worker: &'worker KernelWorker,
    pub(crate) from_version: KernelVersion,
}

impl<'worker> KernelPatch<'worker> {
    /// The kernel that this patch is based on
    fn the_from_version_kernel(&self) -> Kernel<'_> {
        let Self {
            worker,
            from_version,
        } = self;
        Kernel {
            worker,
            version: from_version.clone(),
        }
    }
}

impl<'worker> Fm for KernelPatch<'worker> {
    fn fm(&self, for_action: Action) -> FileMake {
        self.worker.mode().fm(for_action)
    }
}

impl<'worker> GetHashesAndPatch<'worker> for KernelPatch<'worker> {
    const HASHES_SIGNATURE_KEY: GpgKeyRef = KERNEL_INCR_KEY;

    fn worker(&self) -> &'worker KernelWorker {
        self.worker
    }

    fn hashes_and_patches_base_url(&self) -> String {
        self.the_from_version_kernel().kernel_directory_url() + "incr/"
    }

    fn hashes_base_dir(&self) -> Result<Arc<Path>> {
        let path = self.worker().dirs().hashes().append("incr");
        create_dir_if_not_exists(&path)?;
        Ok(path.into())
    }

    fn patches_base_dir(&self) -> Result<Arc<Path>> {
        let path = self.worker().dirs().patches().append("incr");
        create_dir_if_not_exists(&path)?;
        Ok(path.into())
    }

    fn stamps_base_dir(&self) -> Result<Arc<Path>> {
        let path = self.worker().dirs().stamps().append("incr");
        create_dir_if_not_exists(&path)?;
        Ok(path.into())
    }

    fn from_version(&self) -> &KernelVersion {
        &self.from_version
    }

    fn to_version(&self) -> KernelVersion {
        self.from_version.sublevel_add(1)
    }
}
