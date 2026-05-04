//! Allow switching between &Path and &SegmentedPath

use std::{cmp::Ordering, ffi::OsStr, fmt::Debug, path::Path};

use crate::{
    lst::{
        path_cmp,
        segmented_path::{SegmentedPath, SegmentedPathParent},
    },
    shared_regions::SharedRegion,
};

#[derive(Debug, Clone, Copy)]
pub struct PathParent<'region>(pub Option<&'region Path>);

pub trait PossiblySegmentedPath<'region, INLINE>:
    Debug + Send + Sync + Copy
{
    type ParentType: PossiblySegmentedPathParent<'region, INLINE, Self>;

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

    fn keep_as_parent(self) -> Self::ParentType;
    fn drop_as_parent(self) -> Self::ParentType;
}

impl<'region, INLINE> PossiblySegmentedPath<'region, INLINE> for &'region Path {
    type ParentType = PathParent<'region>;

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

    fn keep_as_parent(self) -> Self::ParentType {
        PathParent(Some(self))
    }

    fn drop_as_parent(self) -> Self::ParentType {
        PathParent(None)
    }
}

impl<'region, INLINE> PossiblySegmentedPath<'region, INLINE>
    for &'region SegmentedPath<'region>
{
    type ParentType = SegmentedPathParent<'region>;

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

    fn keep_as_parent(self) -> Self::ParentType {
        SegmentedPathParent(Some(self))
    }

    fn drop_as_parent(self) -> Self::ParentType {
        SegmentedPathParent(None)
    }
}

// ========================================================================================
// And now also need a trait for the parent types. Tying a knot via
// passing in `P` parameter, or the trait solver(?) won't find the
// implementations! (Replacing P with an associated type doesn't
// work.)

pub trait PossiblySegmentedPathParent<
    'region,
    INLINE,
    P: PossiblySegmentedPath<'region, INLINE>,
>: Debug + Send + Sync + Copy
{
    fn psp_add_segment(
        self,
        file_name: &OsStr,
        region: &mut SharedRegion<'region>,
    ) -> P;
}

impl<'region, INLINE>
    PossiblySegmentedPathParent<'region, INLINE, &'region Path>
    for PathParent<'region>
{
    fn psp_add_segment(
        self,
        file_name: &OsStr,
        region: &mut SharedRegion<'region>,
    ) -> &'region Path {
        match self.0 {
            Some(p) => PossiblySegmentedPath::<INLINE>::psp_add_segment(
                p, file_name, region,
            ),
            None => {
                // Allocate file_name as the path
                let bytes = file_name.as_encoded_bytes();
                let len = bytes.len();
                let alloc = region.allocate::<1>(len);
                alloc.copy_from_slice(bytes);
                unsafe {
                    // Safety: should be OK?
                    OsStr::from_encoded_bytes_unchecked(alloc)
                }
                .as_ref()
            }
        }
    }
}

impl<'region, INLINE>
    PossiblySegmentedPathParent<
        'region,
        INLINE,
        &'region SegmentedPath<'region>,
    > for SegmentedPathParent<'region>
{
    fn psp_add_segment(
        self,
        file_name: &OsStr,
        region: &mut SharedRegion<'region>,
    ) -> &'region SegmentedPath<'region> {
        self.add_segment(file_name, region)
    }
}
