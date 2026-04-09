use std::{mem::transmute, slice::from_raw_parts_mut};

pub unsafe fn hack_static<'a, 'v, T: ?Sized>(rf: &'a mut T) -> &'v mut T {
    transmute(rf)
}

#[derive(Debug, Clone, Copy)]
pub struct TRawSliceMut<T> {
    data: *mut T,
    len: usize,
}

impl<T> From<&mut [T]> for TRawSliceMut<T> {
    fn from(sl: &mut [T]) -> Self {
        let data = sl.as_mut_ptr();
        let len = sl.len();
        Self { data, len }
    }
}

impl<T> TRawSliceMut<T> {
    pub unsafe fn to_slice_mut<'t>(self) -> &'t mut [T] {
        let Self { data, len } = self;
        from_raw_parts_mut(data, len)
    }

    pub fn split_at_mut(self, pos: usize) -> (TRawSliceMut<T>, TRawSliceMut<T>) {
        let Self { data, len } = self;
        assert!(pos <= len);
        // unnecessary check?, as data+len must be within usize and
        // thus data+pos, too.
        assert!(pos <= isize::MAX as usize);
        let head = TRawSliceMut { data, len: pos };
        let rest = TRawSliceMut {
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

pub unsafe fn hack_static2<'a, T>(rf: &'a mut [T]) -> TRawSliceMut<T> {
    TRawSliceMut::from(rf)
}

