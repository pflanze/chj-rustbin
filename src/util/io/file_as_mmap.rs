use std::{fs::File, os::unix::fs::MetadataExt, path::PathBuf};

use anyhow::{anyhow, Context, Result};
use memmap2::MmapOptions;

use crate::util::io::mmap::{MiriCompatibleMap, Mmap};

pub struct FileAsMmap {
    pub path: PathBuf,
    pub file: File,
    pub len: usize,
    pub mmap: Mmap,
}

impl FileAsMmap {
    pub fn open(path: PathBuf) -> Result<Self> {
        let file = File::open(&path)
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
        Ok(FileAsMmap {
            path,
            file,
            mmap,
            len,
        })
    }
}
