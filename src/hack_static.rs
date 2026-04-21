use std::mem::transmute;

pub unsafe fn hack_static<'v, T: ?Sized>(rf: &mut T) -> &'v mut T {
    transmute(rf)
}
