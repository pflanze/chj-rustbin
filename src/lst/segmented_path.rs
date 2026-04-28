use std::{
    cmp::Ordering, ffi::OsStr, marker::PhantomPinned, path::Path,
    slice::from_raw_parts, sync::atomic::AtomicPtr,
};

use crate::{
    kitschcell::{KitschCache, KitschCell},
    leaked_region::{GlobalLeakedRegions, LeakedRegion},
};

/// Provide an initial re-usable buffer for path
/// construction. Functions push into this vector; clear it beforehand
/// if necessary.
pub fn tmp_path_buffer() -> Vec<u8> {
    Vec::with_capacity(512)
}

/// A `str` with length provided in the allocation together with the
/// data following after it
#[derive(Debug)]
struct LenStr {
    len: u32,
    _pin: PhantomPinned,
}

impl LenStr {
    /// Must be non-null, and allocated with the str immediately
    /// following it.
    unsafe fn deref<'t>(this: *const Self) -> &'t str {
        unsafe {
            let r: &Self = &*this;
            let this_bytes = this as *const u8;
            let data = this_bytes.add(size_of::<Self>());
            let byteslice = from_raw_parts(data, r.len as usize);
            std::str::from_utf8_unchecked(byteslice)
        }
    }
}

#[derive(Debug)]
pub struct SegmentedPath<'region> {
    parent: Option<&'region SegmentedPath<'region>>,
    depth: u16,

    /// Length of the original file name (OsStr) as bytes (no \0 at
    /// the end); data follows immediately after the struct.
    orig_name_len: u16,

    /// Version of the file name for comparison.  Generated on demand,
    /// nullptr means not generated yet. (Only generated if
    /// `orig_name_len` is longer than 8 B or not ascii, because
    /// otherwise it's faster to regenerate on the fly?)
    lc_name: AtomicPtr<LenStr>,

    // XX temporary for debugging to avoid UB when dumping bytes
    _unused: u32,
    _pin: PhantomPinned,
}

impl<'region> PartialEq for SegmentedPath<'region> {
    fn eq(&self, other: &Self) -> bool {
        self.depth == other.depth
            && self.orig_name_len == other.orig_name_len
            && self.orig_name() == other.orig_name()
            && self.parent == other.parent
    }
}

impl<'region> Eq for SegmentedPath<'region> {}

// Can't implement PartialOrd / Ord since it needs an allocator, use
// SegmentedPath::cmp instead.

// Both must be of the same depth !
fn _segmented_path_ord<'t: 'u, 'u, 'region: 't>(
    p1: &SegmentedPath<'region>,
    p2: &SegmentedPath<'region>,
    get_leaked_region: &mut impl KitschCache<LeakedRegion<'region>>,
) -> Ordering {
    if let Some(p1p) = p1.parent {
        let p2p = p2
            .parent
            .expect("expect that both segmented paths are of the same length");
        _segmented_path_ord(p1p, p2p, get_leaked_region)
            .then_with(|| p1.cmp_file_name(p2, get_leaked_region))
    } else {
        p1.cmp_file_name(p1, get_leaked_region)
    }
}

fn segmented_path_ord<'region>(
    p1: &SegmentedPath<'region>,
    p2: &SegmentedPath<'region>,
    regions: &'region GlobalLeakedRegions,
) -> Ordering {
    let shared_len = p1.depth.min(p2.depth);
    let p1s = p1.take_segments(shared_len).expect("have shared_len");
    let p2s = p2.take_segments(shared_len).expect("have shared_len");

    let mut get_leaked_region =
        KitschCell::Uninitialized(|| -> LeakedRegion<'region> {
            regions.get_region()
        });

    _segmented_path_ord(p1s, p2s, &mut get_leaked_region).then_with(|| {
        // Which path ran out?
        p1.depth.cmp(&p2.depth)
    })
}

impl<'region> SegmentedPath<'region> {
    fn new(
        parent: Option<&'region SegmentedPath<'region>>,
        orig_name: &OsStr,
        leaked_region: &mut LeakedRegion<'region>,
    ) -> &'region Self {
        const STRUCT_SIZE: usize = size_of::<SegmentedPath>();
        const STRUCT_ALIGN: usize = align_of::<SegmentedPath>();

        let orig_name_bytes = orig_name.as_encoded_bytes();
        #[allow(unused)]
        let orig_name = ();
        let orig_name_len = orig_name_bytes.len();

        let alloc_len = STRUCT_SIZE + orig_name_len;
        let alloc = leaked_region.allocate::<STRUCT_ALIGN>(alloc_len);
        let (_alloc_struct, alloc_orig) = alloc.split_at_mut(STRUCT_SIZE);
        alloc_orig.copy_from_slice(orig_name_bytes);

        let depth = match parent {
            Some(d) => d.depth + 1,
            None => 0,
        };
        let slf = Self {
            parent,
            depth,
            orig_name_len: orig_name_len
                .try_into()
                .expect("no path segment can be longer than u16::MAX"),
            lc_name: AtomicPtr::default(),
            _pin: PhantomPinned,
            _unused: 0xcafebabe,
        };

        // MIRI doesn't like us using `_alloc_struct`, thus use `alloc`.
        let self_ptr = alloc.as_mut_ptr() as *mut Self;
        unsafe {
            // SAFETY: We reserved the head for it.
            std::ptr::write(self_ptr, slf);
        }

        // dbg!(&alloc);

        unsafe {
            // SAFETY: We just wrote the value. The lifetime is that
            // of the allocator, 'region, as specified in the return type.
            &*self_ptr
        }
    }

    /// Push another segment at the right (dir item), like
    /// PathBuf::push but functional. Shallow wrapper around `new`.
    pub fn add_segment(
        self: &'region Self,
        orig_name: &OsStr,
        leaked_region: &mut LeakedRegion<'region>,
    ) -> &'region Self {
        Self::new(Some(self), orig_name, leaked_region)
    }

    /// Returns None for the path ""
    pub fn new_from_path(
        path: &Path,
        leaked_region: &mut LeakedRegion<'region>,
    ) -> Option<&'region Self> {
        let mut p = None;
        for segment in path {
            let segment_bytes = segment.as_encoded_bytes();
            let use_segment = if segment_bytes == &[b'/'] {
                &[]
            } else {
                segment_bytes
            };
            let use_segment_osstr: &OsStr = unsafe {
                // SAFETY: back from what we had, or empty, is OK?
                OsStr::from_encoded_bytes_unchecked(use_segment)
            };
            p = Some(SegmentedPath::new(p, use_segment_osstr, leaked_region));
        }
        p
    }

    pub fn orig_name(&self) -> &'region OsStr {
        const STRUCT_SIZE: usize = size_of::<SegmentedPath>();
        let self_ptr: *const Self = self;
        let self_addr = self_ptr as *const u8;
        let bytes_addr = unsafe {
            // Safety: `new` allocates the whole thing in one go, and
            // we don't go past it
            self_addr.add(STRUCT_SIZE)
        };
        let bytes: &[u8] =
            unsafe { from_raw_parts(bytes_addr, self.orig_name_len as usize) };
        unsafe {
            // Safety: we created the bytes via `as_encoded_bytes()`
            // in `new`
            OsStr::from_encoded_bytes_unchecked(bytes)
        }
    }

    /// Take the n+1 left-most path segments (n == 0 returns
    /// `Some(self)`). Returns `None` if n > self.depth.
    pub fn take_segments(&self, n: u16) -> Option<&Self> {
        if n > self.depth {
            None
        } else {
            let dropn = self.depth - n;
            let mut p = self;
            for _ in 0..dropn {
                p = p.parent.expect("checked");
            }
            Some(p)
        }
    }

    /// Appends a '/', always (drop it afterwards).
    fn _to_path(&self, out: &mut Vec<u8>) {
        if let Some(parent) = self.parent {
            parent._to_path(out)
        }
        // XX is encoded_bytes the right format for Path?
        let bytes = self.orig_name().as_encoded_bytes();
        out.extend_from_slice(bytes);
        out.push(b'/');
    }

    /// Generate Path by pushing into `tmp` (clear it
    /// sometimes). Returns the slice that was pushed.
    pub fn to_path<'tmp>(&self, tmp: &'tmp mut Vec<u8>) -> &'tmp Path {
        let i0 = tmp.len();
        self._to_path(tmp);
        let i1 = tmp.len();
        // Path "" is really "/" if there's no other segment
        let used = if (i1 - i0) == 1 {
            &tmp[i0..i1]
        } else {
            &tmp[i0..i1 - 1]
        };
        let osstr = unsafe { OsStr::from_encoded_bytes_unchecked(used) };
        osstr.as_ref()
    }

    /// Version of the file name in unicode lower-case, with
    /// non-alphanumeric characters removed. Generated and cached on
    /// first access.
    pub fn lc_name<'t>(
        &self,
        get_leaked_region: &mut impl KitschCache<LeakedRegion<'region>>,
    ) -> &'region str
    where
        'region: 't,
    {
        // XX optimize?: for lc file name lengths <=
        // `size_of::<self.lc_name>`, store inline in the atomic (as
        // fake pointer)?

        // XX which Ordering ?
        let mut p = self.lc_name.load(std::sync::atomic::Ordering::SeqCst);
        if p.is_null() {
            let orig_name_lossy = self.orig_name().to_string_lossy();

            const STRUCT_SIZE: usize = size_of::<LenStr>();
            const STRUCT_ALIGN: usize = align_of::<LenStr>();
            let alloc_len = STRUCT_SIZE + orig_name_lossy.len() * 4;
            let leaked_region = get_leaked_region.get();
            let alloc = leaked_region.allocate::<STRUCT_ALIGN>(alloc_len);

            let (_alloc_struct, alloc_data) = alloc.split_at_mut(STRUCT_SIZE);

            let mut i = 0;
            for c in orig_name_lossy.chars() {
                if c.is_alphanumeric() {
                    if c.is_ascii() {
                        let cl = c.to_ascii_lowercase() as u8;
                        alloc_data[i] = cl;
                        i += 1;
                    } else {
                        for c in c.to_lowercase() {
                            let encoded = c.encode_utf8(&mut alloc_data[i..]);
                            i += encoded.len();
                        }
                    }
                }
            }

            let len: u32 =
                i.try_into().expect("expect segment name length < u32::MAX");

            // MIRI doesn't like us using `_alloc_struct`, thus use `alloc`.
            p = alloc.as_mut_ptr() as *mut LenStr;
            unsafe {
                // SAFETY: using the space reserved for the struct, of
                // STRUCT_SIZE length, aligned by STRUCT_ALIGN
                p.write(LenStr {
                    len,
                    _pin: PhantomPinned,
                });
            }

            unsafe {
                // SAFETY: STRUCT_SIZE + i bytes were written without
                // panic, hence no overflow; the remainder is unused.
                leaked_region.return_unused(alloc_len - i - STRUCT_SIZE);
            }

            // XX which Ordering ?
            self.lc_name.store(p, std::sync::atomic::Ordering::SeqCst);
        }

        unsafe {
            // Safety: is not null, and properly initialized from
            // above
            LenStr::deref(p)
        }
    }

    /// Can't implement PartialOrd / Ord since it needs an allocator,
    /// hence this method
    pub fn cmp(
        &self,
        other: &Self,
        regions: &'region GlobalLeakedRegions,
    ) -> Ordering {
        segmented_path_ord(self, other, regions)
    }

    /// Compare the file names, only (lower-cased), completely
    /// ignoring the parents
    pub fn cmp_file_name<'t>(
        &self,
        other: &Self,
        get_leaked_region: &mut impl KitschCache<LeakedRegion<'region>>,
    ) -> Ordering
    where
        'region: 't,
    {
        let n1 = self.lc_name(get_leaked_region);
        let n2 = other.lc_name(get_leaked_region);
        n1.cmp(n2).then_with(|| {
            let n1 = self.orig_name();
            let n2 = other.orig_name();
            n1.cmp(n2)
        })
    }

    /// From right to left (i.e. in reverse order)
    pub fn orig_name_segments(&self) -> Vec<&'region OsStr> {
        let mut v = Vec::new();
        let mut p = self;
        loop {
            v.push(p.orig_name());
            if let Some(parent) = p.parent {
                p = parent;
            } else {
                return v;
            }
        }
    }

    /// From right to left (i.e. in reverse order)
    pub fn lc_name_segments(
        &self,
        get_leaked_region: &mut impl KitschCache<LeakedRegion<'region>>,
    ) -> Vec<&'region str> {
        let mut v = Vec::new();
        let mut p = self;
        loop {
            v.push(p.lc_name(get_leaked_region));
            if let Some(parent) = p.parent {
                p = parent;
            } else {
                return v;
            }
        }
    }
}

#[test]
fn t_size() {
    // Should be true even on 32-bit architectures?
    assert_eq!(size_of::<SegmentedPath>(), 24);
}

#[cfg(test)]
mod tests {
    use anyhow::Result;

    use super::*;

    fn p<'region>(
        path: &str,
        region: &mut LeakedRegion<'region>,
    ) -> Option<&'region SegmentedPath<'region>> {
        SegmentedPath::new_from_path(path.as_ref(), region)
    }

    #[test]
    fn t_() -> Result<()> {
        let regions = GlobalLeakedRegions::new(1_000_000);
        let mut ps = {
            let mut get_region =
                KitschCell::Uninitialized(|| -> LeakedRegion {
                    regions.get_region()
                });
            move |path: &str, expected_path2: &str| -> Option<Vec<&str>> {
                let mut region = get_region.get();
                let spath = p(path, &mut region)?;
                let mut tmp = tmp_path_buffer();
                let path2: &str =
                    spath.to_path(&mut tmp).to_str().expect("was str");
                let mut segs: Vec<&str> = spath
                    .orig_name_segments()
                    .into_iter()
                    .map(|s| s.to_str().expect("was str thus still is"))
                    .collect();
                segs.reverse();
                dbg!((path, &segs));
                assert_eq!(path2, expected_path2);
                Some(segs)
            }
        };

        assert_eq!(ps("foo/bar", "foo/bar"), Some(vec!["foo", "bar"]));
        assert_eq!(ps("foo/bar/", "foo/bar"), Some(vec!["foo", "bar"]));
        assert_eq!(
            ps("foo//bar/baz", "foo/bar/baz"),
            Some(vec!["foo", "bar", "baz"])
        );
        assert_eq!(ps("/bar", "/bar"), Some(vec!["", "bar"]));
        assert_eq!(ps("./bar", "./bar"), Some(vec![".", "bar"]));
        assert_eq!(ps("bar", "bar"), Some(vec!["bar"]));
        assert_eq!(ps(".", "."), Some(vec!["."]));
        assert_eq!(ps("/", "/"), Some(vec![""]));
        assert_eq!(ps("//", "/"), Some(vec![""]));
        assert_eq!(ps("", ""), None);
        assert_eq!(ps("bär/lauçh", "bär/lauçh"), Some(vec!["bär", "lauçh"]));

        Ok(())
    }
}
