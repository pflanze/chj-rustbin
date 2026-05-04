//! Allow switching between &Path and &SegmentedPath

use std::{cmp::Ordering, ffi::OsStr, fmt::Debug, path::Path};

use crate::{
    lst::{path_cmp, segmented_path::SegmentedPath},
    shared_regions::SharedRegion,
};

pub trait PossiblySegmentedPath<'region, INLINE>: Debug {
    fn ci_cmp(self, other: Self) -> Ordering;

    fn psp_add_segment(
        self,
        file_name: &OsStr,
        region: &mut SharedRegion<'region>,
    ) -> Self;

    /// Clears `tmp` before use
    fn psp_to_path<'tmp>(self, tmp: &'tmp mut Vec<u8>) -> &'tmp Path
    where
        'region: 'tmp;
}

impl<'region, INLINE> PossiblySegmentedPath<'region, INLINE> for &'region Path {
    fn ci_cmp(self, other: Self) -> Ordering {
        path_cmp::ci_cmp::<INLINE>(self, other)
    }

    fn psp_add_segment(
        self,
        file_name: &OsStr,
        region: &mut SharedRegion<'region>,
    ) -> Self {
        if false {
            let mut p = self.to_owned();
            p.push(file_name);
            region.allocate_path(&p)
        } else {
            // optimize: copy directly to region
            let b0 = self.as_os_str().as_encoded_bytes();
            let b1 = file_name.as_encoded_bytes();
            let l0 = b0.len();
            let l1 = b1.len();
            let len = l0 + 1 + l1;
            let alloc = region.allocate::<1>(len);
            alloc[0..l0].copy_from_slice(b0);
            alloc[l0] = b'/';
            alloc[l0 + 1..len].copy_from_slice(b1);
            unsafe {
                // Safety: should be OK?
                OsStr::from_encoded_bytes_unchecked(alloc)
            }
            .as_ref()
        }
    }

    fn psp_to_path<'tmp>(self, _tmp: &'tmp mut Vec<u8>) -> &'tmp Path
    where
        'region: 'tmp,
    {
        self
    }
}

impl<'region, INLINE> PossiblySegmentedPath<'region, INLINE>
    for &'region SegmentedPath<'region>
{
    fn ci_cmp(self, other: Self) -> Ordering {
        self.cmp(other)
    }

    fn psp_add_segment(
        self,
        file_name: &OsStr,
        region: &mut SharedRegion<'region>,
    ) -> Self {
        self.add_segment(file_name, region)
    }

    fn psp_to_path<'tmp>(self, tmp: &'tmp mut Vec<u8>) -> &'tmp Path
    where
        'region: 'tmp,
    {
        tmp.clear();
        SegmentedPath::to_path(self, tmp)
    }
}
