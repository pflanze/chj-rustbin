use std::cmp::Ordering;

use num::{CheckedSub, Num};

/// Get a function that reports whether two numbers are within maxdiff
/// of each other (either direction).
pub fn numbers_within<N>(
    maxdiff: N
) -> impl Fn(N, N) -> bool
where N: CheckedSub + Num + Ord
{
    move |a: N, b: N| -> bool {
        if a > b {
            a.checked_sub(&b).expect("should always work, am I wrong?") < maxdiff
        } else {
            b.checked_sub(&a).expect("should always work, am I wrong?") < maxdiff
        }
    }
}

/// std::cmp::max doesn't work on floating point
pub fn max_f64(a: f64, b: f64) -> f64 {
    if a < b {
        b
    } else {
        a
    }
}

/// Comparison between two values with only partial ordering
/// (e.g. floating point numbers) that provides a fake full ordering
/// by falling back to Ordering::Equal if non-comparable. Warning:
/// this may not be what you want, but perhaps panic instead?
pub fn forced_cmp<T: PartialOrd>(a: T, b: T) -> Ordering {
    a.partial_cmp(&b).unwrap_or(Ordering::Equal)
}

/// An addition that ignores NaN numbers if possible (the result
/// should only be NaN if both arguments are NaN). In other words, NaN
/// is treated the same as 0, as long as at least one argument isn't
/// NaN. (Warning: this might not be what you want.)
pub fn nandropping_add(a: f64, b: f64) -> f64 {
    if a.is_nan() {
        b
    } else if b.is_nan() {
        a
    } else {
        a + b
    }
}

