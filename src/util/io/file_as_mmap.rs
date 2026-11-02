use std::{
    fs::File,
    os::unix::fs::MetadataExt,
    path::{Path, PathBuf},
};

use anyhow::{anyhow, Context, Result};
use memmap2::MmapOptions;
use once_cell::sync::OnceCell;

use crate::util::io::mmap::{MiriCompatibleMap, Mmap};

pub struct FileAsMmap {
    path: PathBuf,
    len: OnceCell<usize>,
    mmap: OnceCell<Mmap>,
}

impl FileAsMmap {
    /// Only the path is stored, the file is not opened yet
    pub fn new(path: PathBuf) -> Self {
        FileAsMmap {
            path,
            len: Default::default(),
            mmap: Default::default(),
        }
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Try to get the (current) length of the file
    ///
    /// Note: this value is cached, and will also be used by the
    /// `FileAsMmap::len` method. Thus be careful not to make the file
    /// smaller between calling `len` and then `mmap`.
    pub fn len(&self) -> Result<usize> {
        self.len
            .get_or_try_init(|| {
                let path = self.path();
                let metadata = path.metadata().with_context(|| {
                    anyhow!("getting metadata for file {path:?}")
                })?;
                let file_size = metadata.size();
                let len: usize = file_size.try_into().with_context(|| {
                    anyhow!(
                        "converting file size {file_size} \
                         to mmap size for file {path:?}"
                    )
                })?;
                Ok(len)
            })
            .copied()
    }

    /// Open the file and try to map its memory, then close it again
    /// (to avoid too many open filehandles). Uses/forces the cached
    /// value of the `FileAsMmap::len` method.
    pub fn mmap(&self) -> Result<&Mmap> {
        self.mmap.get_or_try_init(|| {
            let len = self.len()?;
            let path = self.path();
            let file = File::open(path)
                .with_context(|| anyhow!("opening file {path:?}"))?;

            let page_bits = if len > 1_600_000 {
                // On Linux at least: "The number 21 corresponds to
                // `MAP_HUGE_2MB`. See mmap(2) for more details."
                Some(21)
            } else {
                // Otherwise use the default small page size; less memory
                // waste and not too costly since it's small, hence few
                // pages.
                None
            };
            let mmap = unsafe {
                // Safety: mostly safe since the app doesn't overwrite
                // files (it only ever appends, hence a mmap should never
                // be invalidated by the app itself)
                MmapOptions::new()
                    .len(len)
                    .huge(page_bits)
                    .miri_compatible_map(&file)
            }
            .with_context(|| anyhow!("memory-mapping file {path:?}"))?;

            Ok(mmap)
        })
    }
}
