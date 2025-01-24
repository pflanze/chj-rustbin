
/// Make the lifetime of a reference static. Be very careful with your
/// reasoning why your usage doesn't introduce UB!
pub unsafe fn statify<T>(r: &T) -> &'static T {
    let ptr: *const T = r;
    &*ptr
}

/// Same as `statify` but for mutable references.
pub unsafe fn statify_mut<T>(r: &mut T) -> &'static mut T {
    let ptr: *mut T = r;
    &mut *ptr
}

