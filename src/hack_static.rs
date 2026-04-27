use std::mem::transmute;

#[allow(clippy::needless_lifetimes)]
pub unsafe fn hack_static<'a, 'v, T: ?Sized>(rf: &'a mut T) -> &'v mut T {
    transmute(rf)
}

#[allow(clippy::needless_lifetimes)]
pub unsafe fn hack_static_const<'a, 'v, T: ?Sized>(rf: &'a T) -> &'v T {
    transmute(rf)
}
