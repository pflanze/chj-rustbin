//! Wrapper around the `memmap2` crate or, for Miri compatibility, normal file reads

use std::fs::File;

use anyhow::Result;
pub use memmap2::MmapOptions;

pub trait MiriCompatibleMap {
    unsafe fn miri_compatible_map(&self, file: &File) -> Result<Mmap>;
}

#[cfg(not(miri))]
mod normal {
    use std::fs::File;

    pub use memmap2::Mmap;
    use memmap2::MmapOptions;

    use crate::util::io::mmap::MiriCompatibleMap;

    impl MiriCompatibleMap for MmapOptions {
        unsafe fn miri_compatible_map(
            &self,
            file: &File,
        ) -> anyhow::Result<Mmap> {
            unsafe { self.map(file) }.map_err(Into::into)
        }
    }
}

#[cfg(miri)]
mod miri {
    use std::{fs::File, io::Read, ops::Deref};

    use memmap2::MmapOptions;

    use crate::util::io::mmap::MiriCompatibleMap;

    pub struct Mmap {
        data: Vec<u8>,
    }

    impl Deref for Mmap {
        type Target = [u8];

        fn deref(&self) -> &Self::Target {
            &self.data
        }
    }

    impl AsRef<[u8]> for Mmap {
        fn as_ref(&self) -> &[u8] {
            &self.data
        }
    }

    impl MiriCompatibleMap for MmapOptions {
        unsafe fn miri_compatible_map(
            &self,
            mut file: &File,
        ) -> anyhow::Result<Mmap> {
            let mut data = Vec::new();
            file.read_to_end(&mut data)?;
            Ok(Mmap { data })
        }
    }
}

#[cfg(not(miri))]
pub use normal::Mmap;

#[cfg(miri)]
pub use miri::Mmap;
