//! Allow switching between &Path and &SegmentedPath

use std::{cmp::Ordering, ffi::OsStr, fmt::Debug, path::Path};

use crate::{
    leaked_region::{GlobalLeakedRegions, LeakedRegion},
    lst::{path_cmp, segmented_path::SegmentedPath},
};

pub trait PossiblySegmentedPath<'region, INLINE>: Debug {
    fn ci_cmp(
        self,
        other: Self,
        regions: &'region GlobalLeakedRegions,
    ) -> Ordering;

    fn psp_add_segment(
        self,
        file_name: &OsStr,
        region: &mut LeakedRegion<'region>,
    ) -> Self;

    fn psp_to_path<'tmp>(self, tmp: &'tmp mut Vec<u8>) -> &'tmp Path
    where
        'region: 'tmp;
}

impl<'region, INLINE> PossiblySegmentedPath<'region, INLINE> for &'region Path {
    fn ci_cmp(
        self,
        other: Self,
        _regions: &'region GlobalLeakedRegions,
    ) -> Ordering {
        path_cmp::ci_cmp::<INLINE>(self, other)
    }

    fn psp_add_segment(
        self,
        file_name: &OsStr,
        region: &mut LeakedRegion<'region>,
    ) -> Self {
        // XX optimize: copy directly to region! Will see the cost now.
        let mut p = self.to_owned();
        p.push(file_name);
        region.allocate_path(&p)
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
    fn ci_cmp(
        self,
        other: Self,
        regions: &'region GlobalLeakedRegions,
    ) -> Ordering {
        self.cmp(other, regions)
    }

    fn psp_add_segment(
        self,
        file_name: &OsStr,
        region: &mut LeakedRegion<'region>,
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
