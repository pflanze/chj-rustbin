/// This is a polyfill for rustc 1.48.0

use std::cmp::Ordering;

pub trait MoreOrdering {
    fn is_ge(self) -> bool;
}

impl MoreOrdering for Ordering {
    fn is_ge(self) -> bool {
        match self {
            Ordering::Greater => true,
            Ordering::Equal => true,
            Ordering::Less => false,
        }
    }
}
