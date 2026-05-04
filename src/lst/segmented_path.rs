use std::{
    cmp::Ordering, ffi::OsStr, marker::PhantomPinned, path::Path,
    slice::from_raw_parts,
};

use num::Zero;

use crate::shared_regions::SharedRegion;

fn pointer_eq<T>(a: &T, b: &T) -> bool {
    let a: *const T = a;
    let b: *const T = b;
    a == b
}

fn pointer_eq_opt<T>(a: Option<&T>, b: Option<&T>) -> bool {
    let a: *const T = a.map_or(std::ptr::null(), |r| r);
    let b: *const T = b.map_or(std::ptr::null(), |r| r);
    a == b
}

/// Provide an initial re-usable buffer for path
/// construction. Functions push into this vector; clear it beforehand
/// if necessary.
pub fn tmp_path_buffer() -> Vec<u8> {
    Vec::with_capacity(512)
}

#[derive(Debug)]
pub struct SegmentedPath<'region> {
    parent: Option<&'region SegmentedPath<'region>>,
    depth: u16,

    /// Length of the version of the file name for comparison; data
    /// follows immediately after the struct.
    lc_name_len: u32,

    /// Length of the original file name (OsStr) as bytes (no \0 at
    /// the end); data follows immediately after the `lc_name`
    /// data. If identical to `lc_name`, the original file name is not
    /// stored and `orig_name_len` is set to 0.
    orig_name_len: u16,

    _pin: PhantomPinned,
}

impl<'region> PartialEq for SegmentedPath<'region> {
    fn eq(&self, other: &Self) -> bool {
        pointer_eq_opt(self.parent, other.parent)
            && self.orig_name() == other.orig_name()
    }
}

impl<'region> Eq for SegmentedPath<'region> {}

// Both must be of the same depth !
fn _segmented_path_ord<'t: 'u, 'u, 'region: 't>(
    p1: &SegmentedPath<'region>,
    p2: &SegmentedPath<'region>,
) -> Ordering {
    if pointer_eq(p1, p2) {
        return Ordering::Equal;
    }
    if let Some(p1p) = p1.parent {
        let p2p = p2
            .parent
            .expect("expect that both segmented paths are of the same length");
        _segmented_path_ord(p1p, p2p).then_with(|| p1.cmp_file_name(p2))
    } else {
        p1.cmp_file_name(p1)
    }
}

fn segmented_path_ord<'region>(
    p1: &SegmentedPath<'region>,
    p2: &SegmentedPath<'region>,
) -> Ordering {
    // XX do we need to check pointers first for optim?
    if pointer_eq(p1, p2) {
        unreachable!("we ASSUME that sort never does that?");
        // return Ordering::Equal;
    }

    let shared_len = p1.depth.min(p2.depth);
    let p1s = p1.take_segments(shared_len).expect("have shared_len");
    let p2s = p2.take_segments(shared_len).expect("have shared_len");

    _segmented_path_ord(p1s, p2s).then_with(|| {
        // Which path ran out?
        p1.depth.cmp(&p2.depth)
    })
}

/// Store those characters compared via `ci_cmp`. Returns a reference
/// to the stored data, and the remainder of `alloc`.
fn store_ci_cmp_chars<'a>(
    s: &str,
    alloc: &'a mut [u8],
) -> (&'a mut [u8], &'a mut [u8]) {
    let mut i = 0;
    for c in s.chars() {
        if c.is_alphanumeric() {
            if c.is_ascii() {
                let cl = c.to_ascii_lowercase() as u8;
                alloc[i] = cl;
                i += 1;
            } else {
                for c in c.to_lowercase() {
                    let encoded = c.encode_utf8(&mut alloc[i..]);
                    i += encoded.len();
                }
            }
        }
    }
    alloc.split_at_mut(i)
}

impl<'region> SegmentedPath<'region> {
    fn new(
        parent: Option<&'region SegmentedPath<'region>>,
        orig_name: &OsStr,
        shared_region: &mut SharedRegion<'region>,
    ) -> &'region Self {
        const STRUCT_SIZE: usize = size_of::<SegmentedPath>();
        const STRUCT_ALIGN: usize = align_of::<SegmentedPath>();

        let orig_name_bytes = orig_name.as_encoded_bytes();
        let orig_name_len = orig_name_bytes.len();

        // The largest possible allocation that we need
        let alloc_len = STRUCT_SIZE + orig_name_len * 9; // 1 orig, 2 unicode chars for lc
        let alloc = shared_region.allocate::<STRUCT_ALIGN>(alloc_len);
        let (_alloc_struct, tail) = alloc.split_at_mut(STRUCT_SIZE);

        let (lc_name, tail) = {
            let orig_name_lossy = orig_name.to_string_lossy();
            store_ci_cmp_chars(&orig_name_lossy, tail)
        };

        #[allow(unused)]
        let orig_name = ();

        let (orig_name_len, rest) = if lc_name == orig_name_bytes {
            // The two file name versions are identical; do not store
            // `orig_name` as a second copy.
            (0, tail)
        } else {
            let (alloc_orig, rest) = tail.split_at_mut(orig_name_len);
            alloc_orig.copy_from_slice(orig_name_bytes);
            (
                orig_name_len
                    .try_into()
                    .expect("no path segment can be longer than u16::MAX"),
                rest,
            )
        };

        let depth = match parent {
            Some(d) => d.depth + 1,
            None => 0,
        };

        let lc_name_len = lc_name.len();

        let slf = Self {
            parent,
            depth,
            lc_name_len: lc_name_len
                .try_into()
                .expect("no path segment, even after UTF-8 lowercasing, can be longer than u32::MAX"),
            orig_name_len,
            _pin: PhantomPinned,
        };

        let used = STRUCT_SIZE + lc_name_len + usize::from(orig_name_len);
        debug_assert_eq!(used + rest.len(), alloc_len);
        unsafe {
            shared_region.return_unused(rest.len());
        }

        // MIRI doesn't like us using `_alloc_struct`, thus use `alloc`.
        let self_ptr = alloc.as_mut_ptr() as *mut Self;
        unsafe {
            // SAFETY: We reserved the head for it.
            std::ptr::write(self_ptr, slf);
        }

        // dbg!(&alloc[0..used]);

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
        shared_region: &mut SharedRegion<'region>,
    ) -> &'region Self {
        Self::new(Some(self), orig_name, shared_region)
    }

    /// Returns None for the path ""
    pub fn new_from_path(
        path: &Path,
        shared_region: &mut SharedRegion<'region>,
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
            p = Some(SegmentedPath::new(p, use_segment_osstr, shared_region));
        }
        p
    }

    /// Version of the file name in unicode lower-case, with
    /// non-alphanumeric characters removed.
    // Follows immediately after the struct
    pub fn lc_name<'t>(&self) -> &'region str
    where
        'region: 't,
    {
        const STRUCT_SIZE: usize = size_of::<SegmentedPath>();
        let self_ptr: *const Self = self;
        let self_addr = self_ptr as *const u8;
        let bytes_addr = unsafe {
            // Safety: `new` allocates the whole thing in one go, and
            // we don't go past it
            self_addr.add(STRUCT_SIZE)
        };
        let bytes: &[u8] =
            unsafe { from_raw_parts(bytes_addr, self.lc_name_len as usize) };
        unsafe {
            // Safety: we created the bytes as UTF-8 in `new`
            std::str::from_utf8_unchecked(bytes)
        }
    }

    /// The original file name.
    // Follows after lc_name
    pub fn orig_name(&self) -> &'region OsStr {
        if self.orig_name_len.is_zero() {
            return self.lc_name().as_ref();
        }

        const STRUCT_SIZE: usize = size_of::<SegmentedPath>();
        let self_ptr: *const Self = self;
        let self_addr = self_ptr as *const u8;
        let bytes_addr = unsafe {
            // Safety: `new` allocates the whole thing in one go, and
            // we don't go past it
            self_addr.add(STRUCT_SIZE).add(self.lc_name_len as usize)
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

    /// Generate Path by pushing into `tmp` (clears it).
    /// Returns the slice that was pushed.
    pub fn to_path<'tmp>(&self, tmp: &'tmp mut Vec<u8>) -> &'tmp Path {
        tmp.clear();
        self._to_path(tmp);
        // Path "" is really "/" if there's no other segment
        let len = tmp.len();
        let used = if len == 1 { tmp } else { &tmp[0..len - 1] };
        let osstr = unsafe { OsStr::from_encoded_bytes_unchecked(used) };
        osstr.as_ref()
    }

    /// In the past needed an allocator, hence didn't implement
    /// PartialOrd / Ord; XX change now?
    pub fn cmp(&self, other: &Self) -> Ordering {
        segmented_path_ord(self, other)
    }

    /// Compare the file names, only (lower-cased), completely
    /// ignoring the parents
    pub fn cmp_file_name<'t>(&self, other: &Self) -> Ordering
    where
        'region: 't,
    {
        // XX do we need to check pointers first for optim?
        if pointer_eq(self, other) {
            unreachable!()
        }

        let n1 = self.lc_name();
        let n2 = other.lc_name();
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
    pub fn lc_name_segments(&self) -> Vec<&'region str> {
        let mut v = Vec::new();
        let mut p = self;
        loop {
            v.push(p.lc_name());
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
    assert_eq!(size_of::<SegmentedPath>(), 16);
}

#[cfg(test)]
mod tests {
    use anyhow::Result;

    use crate::{
        file_location,
        kitschcell::{KitschCache, KitschCell},
        shared_regions::SharedRegions,
    };

    use super::*;

    fn p<'region>(
        path: &str,
        region: &mut SharedRegion<'region>,
    ) -> Option<&'region SegmentedPath<'region>> {
        SegmentedPath::new_from_path(path.as_ref(), region)
    }

    #[test]
    fn t_() -> Result<()> {
        let regions = SharedRegions::new(1_000_000, file_location!(), false);
        let mut ps = {
            let mut get_region = KitschCell::Unevaluated(|| -> SharedRegion {
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
