//! Allow switching between &Path and &SegmentedPath

use std::{cmp::Ordering, fmt::Debug, path::Path};

use crate::{
    leaked_region::GlobalLeakedRegions,
    lst::{path_cmp, segmented_path::SegmentedPath},
};

pub trait PossiblySegmentedPath<'region, INLINE>: Debug {
    fn ci_cmp(
        self,
        other: Self,
        regions: &'region GlobalLeakedRegions,
    ) -> Ordering;

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

    fn psp_to_path<'tmp>(self, tmp: &'tmp mut Vec<u8>) -> &'tmp Path
    where
        'region: 'tmp,
    {
        tmp.clear();
        SegmentedPath::to_path(self, tmp)
    }
}
