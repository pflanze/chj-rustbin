use std::mem::transmute;

pub unsafe fn hack_static<'a, 'v, T: ?Sized>(rf: &'a mut T) -> &'v mut T {
    transmute(rf)
}
