//! Allow switching between &Path and &SegmentedPath

use std::{
    cmp::Ordering, ffi::OsStr, fmt::Debug, os::unix::prelude::OsStrExt,
    path::Path,
};

use crate::{
    lst::{
        path_cmp,
        segmented_path::{SegmentedPath, SegmentedPathParent},
    },
    shared_regions::{NonCollecting, SharedRegion},
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
        region: &mut SharedRegion<'region, NonCollecting>,
    ) -> Self;

    /// Clears `tmp` before use. Strips `omit_base_path` if given (pass None for
    /// using the path for file operations, or a base path if wanting to leave
    /// it out for display); may panic if it is not actually a prefix of `self`,
    /// or not strip anything.
    fn psp_to_path<'tmp>(
        self,
        omit_base_path: Option<Self>,
        tmp: &'tmp mut Vec<u8>,
    ) -> &'tmp Path
    where
        'region: 'tmp;
}

impl<'region, INLINE> PossiblySegmentedPath<'region, INLINE> for &'region Path {
    type ParentType = PathParent<'region>;

    fn ci_cmp(self, other: Self) -> Ordering {
        path_cmp::ci_cmp::<INLINE>(self, other)
    }

    fn psp_add_segment(
        self,
        file_name: &OsStr,
        region: &mut SharedRegion<'region, NonCollecting>,
    ) -> Self {
        if false {
            let mut p = self.to_owned();
            p.push(file_name);
            region.allocate_path(&p)
        } else {
            // optimize: copy directly to region
            let b0 = self.as_os_str().as_bytes();
            let b1 = file_name.as_bytes();
            let l0 = b0.len();
            let l1 = b1.len();
            let len = l0 + 1 + l1;
            let alloc = region.allocate::<1>(len);
            alloc[0..l0].copy_from_slice(b0);
            alloc[l0] = b'/';
            alloc[l0 + 1..len].copy_from_slice(b1);
            // Safety: should be OK?
            OsStr::from_bytes(alloc).as_ref()
        }
    }

    fn psp_to_path<'tmp>(
        self,
        omit_base_path: Option<Self>,
        _tmp: &'tmp mut Vec<u8>,
    ) -> &'tmp Path
    where
        'region: 'tmp,
    {
        if let Some(base_path) = omit_base_path {
            let stripped = self.strip_prefix(base_path).expect(
                "the given omit_base_path must be a prefix of the path to shorten",
            );
            // Really have to replace "" with "."!
            let empty: &Path = "".as_ref();
            if stripped == empty {
                ".".as_ref()
            } else {
                stripped
            }
        } else {
            self
        }
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
        region: &mut SharedRegion<'region, NonCollecting>,
    ) -> Self {
        self.add_segment(file_name, region)
    }

    fn psp_to_path<'tmp>(
        self,
        omit_base_path: Option<Self>,
        tmp: &'tmp mut Vec<u8>,
    ) -> &'tmp Path
    where
        'region: 'tmp,
    {
        tmp.clear();
        if let Some(base_path) = omit_base_path {
            SegmentedPath::to_path_from_base(self, base_path, tmp)
        } else {
            SegmentedPath::to_path(self, tmp)
        }
    }
}

#[test]
fn t_path_strip_prefix() {
    use crate::{
        file_location, lst::segmented_path::tmp_path_buffer,
        shared_regions::SharedRegions,
    };

    struct Inline;
    fn t<'t, Inline, P: PossiblySegmentedPath<'t, Inline> + Copy>(
        a: P,
        b: P,
        expected1: &Path,
        expected2: &Path,
    ) {
        let mut tmp = tmp_path_buffer();

        let got = b.psp_to_path(Some(a), &mut tmp);
        assert_eq!(got, expected1);

        let got = b.psp_to_path(Some(b), &mut tmp);
        assert_eq!(got, expected2);
    }

    let a: &Path = "foo/bar".as_ref();
    let b: &Path = "foo/bar/baz".as_ref();
    let relative: &Path = "baz".as_ref();
    let dot: &Path = ".".as_ref();
    eprintln!("psp_to_path for &Path");
    t::<Inline, _>(a, b, relative, dot);

    let shared_regions = SharedRegions::new(10000, file_location!(), false);
    let mut reg = shared_regions.get_region();
    let aseg = SegmentedPath::new_from_path(a, &mut reg).unwrap();

    // This will allocate the shared base independently, thus not detect it when
    // serializing:
    let bseg = SegmentedPath::new_from_path(b, &mut reg).unwrap();
    eprintln!("psp_to_path for SegmentedPath indep");
    t::<Inline, _>(aseg, bseg, b, dot);

    // The intended use case: shared base also in storage.
    let bseg = aseg.add_segment("baz".as_ref(), &mut reg);
    eprintln!("psp_to_path for SegmentedPath dep");
    t::<Inline, _>(aseg, bseg, relative, dot);
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
        region: &mut SharedRegion<'region, NonCollecting>,
    ) -> P;
}

impl<'region, INLINE>
    PossiblySegmentedPathParent<'region, INLINE, &'region Path>
    for PathParent<'region>
{
    fn psp_add_segment(
        self,
        file_name: &OsStr,
        region: &mut SharedRegion<'region, NonCollecting>,
    ) -> &'region Path {
        match self.0 {
            Some(p) => PossiblySegmentedPath::<INLINE>::psp_add_segment(
                p, file_name, region,
            ),
            None => {
                // Allocate file_name as the path
                let bytes = file_name.as_bytes();
                let len = bytes.len();
                let alloc = region.allocate::<1>(len);
                alloc.copy_from_slice(bytes);
                // Safety: should be OK?
                OsStr::from_bytes(alloc).as_ref()
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
        region: &mut SharedRegion<'region, NonCollecting>,
    ) -> &'region SegmentedPath<'region> {
        self.add_segment(file_name, region)
    }
}
