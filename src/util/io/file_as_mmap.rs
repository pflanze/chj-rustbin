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
    len_and_mmap: OnceCell<(usize, Mmap)>,
}

impl FileAsMmap {
    /// Only the path is stored, the file is not opened yet
    pub fn new(path: PathBuf) -> Self {
        FileAsMmap {
            path,
            len_and_mmap: Default::default(),
        }
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Opens the file and tries to map its memory, then closes it
    /// again (to avoid too many open filehandles).
    pub fn len_and_mmap(&self) -> Result<&(usize, Mmap)> {
        self.len_and_mmap.get_or_try_init(|| {
            let path = self.path();
        let file = File::open(path)
            .with_context(|| anyhow!("opening file {path:?}"))?;
        let metadata = file
            .metadata()
            .with_context(|| anyhow!("getting metadata for file {path:?}"))?;
        let file_size = metadata.size();
        let len: usize = file_size.try_into().with_context(||
            anyhow!("converting file size {file_size} to mmap size for file {path:?}"))?;

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

            Ok((len, mmap))
        })
    }

    /// Wrapper around `FileAsMmap::len_and_mmap`
    pub fn len(&self) -> Result<usize> {
        Ok(self.len_and_mmap()?.0)
    }

    /// Wrapper around `FileAsMmap::len_and_mmap`
    pub fn mmap(&self) -> Result<&Mmap> {
        Ok(&self.len_and_mmap()?.1)
    }
}
