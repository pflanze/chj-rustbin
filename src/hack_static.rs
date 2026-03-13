pub unsafe fn hack_static<'a, 'v, T: ?Sized>(rf: &'a mut T) -> &'v mut T {
    let ptr: *mut T = rf;
    &mut *ptr
}
