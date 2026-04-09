
use std::{
    marker::PhantomData, mem::transmute, ops::Index, slice::from_raw_parts_mut,
};

pub unsafe fn hack_static<'a, 'v, T: ?Sized>(rf: &'a mut T) -> &'v mut T {
    transmute(rf)
}

pub unsafe fn hack_lifetime_via_pointer<'a: 'v, 'v, T>(
    rf: &'a mut [T],
) -> &'v mut [T] {
    let len = rf.len();
    let ptr = rf.as_mut_ptr();
    dbg!((ptr, len));
    unsafe { from_raw_parts_mut(ptr, len) }
}

pub struct ConsSlice<'a, T> {
    handed_out_until_index: usize,
    ptr: *mut T,
    len: usize,
    _lifetime: PhantomData<&'a ()>,
}

unsafe impl<'a, T> Send for ConsSlice<'a, T> {}
unsafe impl<'a, T> Sync for ConsSlice<'a, T> {}

impl<'a, T> From<&'a mut [T]> for ConsSlice<'a, T> {
    fn from(value: &'a mut [T]) -> Self {
        let ptr = value.as_mut_ptr();
        let len = value.len();
        ConsSlice {
            handed_out_until_index: 0,
            ptr,
            len,
            _lifetime: PhantomData,
        }
    }
}

impl<'a, T> Index<usize> for ConsSlice<'a, T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        let Self {
            handed_out_until_index,
            ptr,
            len,
            _lifetime,
        } = self;
        assert!(index < *len);
        assert!(index >= *handed_out_until_index);
        unsafe {
            let p = ptr.add(index);
            &*p
        }
    }
}

impl<'a, T> ConsSlice<'a, T> {
    pub fn slice_from_to<'s>(
        &'s mut self,
        from: usize,
        to: usize,
    ) -> &'a mut [T]
    where
        'a: 's,
    {
        assert!(from <= to);
        let Self {
            handed_out_until_index,
            ptr,
            len,
            _lifetime,
        } = self;
        assert!(from >= *handed_out_until_index);
        assert!(to <= *len);
        *handed_out_until_index = to;
        unsafe { from_raw_parts_mut(ptr.add(from), to - from) }
    }
}
