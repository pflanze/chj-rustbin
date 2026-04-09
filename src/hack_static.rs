use std::{mem::transmute, slice::from_raw_parts_mut};

pub unsafe fn hack_static<'a, 'v, T: ?Sized>(rf: &'a mut T) -> &'v mut T {
    transmute(rf)
}

pub unsafe fn hack_lifetime_via_pointer<'a, 'v, T>(
    rf: &'a mut [T],
) -> &'v mut [T] {
    let len = rf.len();
    let ptr = rf.as_mut_ptr();
    dbg!((ptr, len));
    unsafe { from_raw_parts_mut(ptr, len) }
}
