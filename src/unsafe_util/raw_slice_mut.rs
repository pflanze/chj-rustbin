use std::slice::from_raw_parts_mut;

#[derive(Debug, Clone, Copy)]
pub struct RawSliceMut {
    data: *mut u8,
    len: usize,
}

impl From<&mut [u8]> for RawSliceMut {
    fn from(sl: &mut [u8]) -> Self {
        let data = sl.as_mut_ptr();
        let len = sl.len();
        Self { data, len }
    }
}

impl RawSliceMut {
    pub fn data(&self) -> *mut u8 {
        self.data
    }

    pub fn len(&self) -> usize {
        self.len
    }

    /// Decreases the start of the slice, while retaining the end
    /// (i.e. increase the lenght by `num_bytes`, too). Does not check
    /// correctness (wraps)!
    pub fn start_sub(self, num_bytes: usize) -> Self {
        let Self { data, len } = self;
        Self {
            data: data.wrapping_sub(num_bytes),
            len: len.wrapping_add(len),
        }
    }

    // Silence clippy: why take &mut when the type is Copy--which is
    // the whole point of the unsafe.
    #[allow(clippy::wrong_self_convention)]
    pub unsafe fn to_slice_mut<'t>(self) -> &'t mut [u8] {
        let Self { data, len } = self;
        from_raw_parts_mut(data, len)
    }

    pub fn aligned<const ALIGN: usize>(self) -> Option<Self> {
        const {
            assert!(ALIGN.count_ones() == 1);
        }
        let Self { data, len } = self;
        let offset = data.align_offset(ALIGN);
        if offset < len {
            let data = unsafe {
                // SAFETY: checked above that it lies within the
                // allocation. The pointer type is `*mut u8`, hence
                // offsets are in bytes, as is `ALIGN`.
                data.add(offset)
            };
            let len = len - offset;
            Some(Self { data, len })
        } else {
            None
        }
    }

    pub fn split_at_mut(self, pos: usize) -> (RawSliceMut, RawSliceMut) {
        let Self { data, len } = self;
        assert!(pos <= len);
        // unnecessary check?, as data+len must be within usize and
        // thus data+pos, too.
        assert!(pos <= isize::MAX as usize);
        let head = RawSliceMut { data, len: pos };
        let rest = RawSliceMut {
            data: unsafe {
                // Assuming that `self` is valid, this is safe because
                // pos is within len as checked above, and it is
                // within isize as checked above or as implied by
                // `self` being valid.
                data.add(pos)
            },
            len: len - pos,
        };
        (head, rest)
    }
}
