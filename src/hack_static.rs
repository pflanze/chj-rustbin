use std::{
    cell::UnsafeCell,
    mem::transmute,
    ops::{Deref, RangeFrom},
    slice::from_raw_parts_mut,
};

// std uses : Sync
pub struct MySyncUnsafeCell<T>(UnsafeCell<T>);

unsafe impl<T> Sync for MySyncUnsafeCell<T> {}

impl<T> MySyncUnsafeCell<T> {
    pub fn into_inner(self) -> T {
        self.0.into_inner()
    }

    pub unsafe fn set(&self, val: T) {
        let ptr = self.0.get();
        unsafe { *ptr = val };
    }

    pub unsafe fn get_mut(&self) -> &mut T {
        let ptr = self.0.get();
        unsafe { &mut *ptr }
    }
}

impl<T> From<T> for MySyncUnsafeCell<T> {
    fn from(value: T) -> Self {
        MySyncUnsafeCell(UnsafeCell::from(value))
    }
}

impl<T> Deref for MySyncUnsafeCell<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // evil XXX
        unsafe { &*self.0.get() }
    }
}

pub unsafe fn hack_static<'a, 'v, T: ?Sized>(rf: &'a mut T) -> &'v mut T {
    transmute(rf)
}

#[derive(Debug)]
pub struct TRawSliceMut<T> {
    data: *mut T,
    len: usize,
}

impl<T> Clone for TRawSliceMut<T> {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            len: self.len.clone(),
        }
    }
}

impl<T> Copy for TRawSliceMut<T> {}

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

    pub fn split_at_mut(
        self,
        pos: usize,
    ) -> (TRawSliceMut<T>, TRawSliceMut<T>) {
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

    pub fn subslice(self, index: RangeFrom<usize>) -> Self {
        let start = index.start;
        let Self { data, len } = self;
        let data = unsafe { data.add(start) };
        let len = len - start;
        Self { data, len }
    }
}

impl<T> Iterator for TRawSliceMut<T> {
    type Item = *mut T;

    fn next(&mut self) -> Option<Self::Item> {
        let Self { data, len } = self;
        if *len == 0 {
            None
        } else {
            let p = *data;
            *len -= 1;
            *data = unsafe { data.add(1) };
            Some(p)
        }
    }
}

// impl<T> Index<RangeFrom<usize>> for TRawSliceMut<T> {
//     type Output = ;

//     fn index(&self, index: RangeFrom<usize>) -> &Self::Output {
//         todo!()
//     }
// }

// impl<T> IndexMut<RangeFrom<usize>> for TRawSliceMut<T> {
//     fn index_mut(&mut self, index: RangeFrom<usize>) -> &mut Self::Output {
//         let start = index.start;
//         let Self { data, len }= self;
//         let data = unsafe { data.add(start) };
//         let len = *len - start;
//         todo!()
//     }
// }

pub unsafe fn hack_static2<'a, T>(rf: &'a mut [T]) -> TRawSliceMut<T> {
    TRawSliceMut::from(rf)
}
