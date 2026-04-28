//! Compare two filesystem paths
//!
//! In about the same way the GNU `ls`
//! tool does (case-insensitive and ignoring non-alphanumeric
//! characters).
//!
//! Note: also see segmented_path.rs and possibly_segmented_path.rs

use std::{cmp::Ordering, path::Path, str::Chars};

/// Bypasses slow unicode comparison if both chars are ASCII.
pub fn fast_lowercase_cmp<INLINE>(a: char, b: char) -> Ordering {
    if a.is_ascii() && b.is_ascii() {
        a.to_ascii_lowercase().cmp(&b.to_ascii_lowercase())
    } else {
        a.to_lowercase().cmp(b.to_lowercase())
    }
}

/// Compare two paths case insensitively (as per unicode), while
/// ignoring non-alphanumeric characters. Does not allocate, and
/// optimizes the ASCII case. Warning: if either path is not in
/// unicode encoding, falls back to Path's default Ord comparison!
pub fn ci_cmp<INLINE>(a: &Path, b: &Path) -> Ordering {
    if let Some(a) = a.to_str() {
        if let Some(b) = b.to_str() {
            fn filter<'t>(it: Chars<'t>) -> impl Iterator<Item = char> + 't {
                it.filter(|c| c.is_alphanumeric())
            }
            let mut achars = filter(a.chars());
            let mut bchars = filter(b.chars());
            // What the ordering is case sensitively while still
            // ignoring non-alphanumeric characters
            let mut stricter_ordering = Ordering::Equal;
            loop {
                if let Some(ac) = achars.next() {
                    if let Some(bc) = bchars.next() {
                        match fast_lowercase_cmp::<INLINE>(ac, bc) {
                            Ordering::Equal => {
                                if stricter_ordering == Ordering::Equal {
                                    // ac == bc in lower-case; now
                                    // order lower-case versions
                                    // *before* upper-case ones--the
                                    // whole point of the
                                    // stricter_ordering variable over
                                    // just doing `a.cmp(b)` in the
                                    // end
                                    stricter_ordering = bc.cmp(&ac);
                                }
                            }
                            v => return v,
                        }
                    } else {
                        return Ordering::Greater;
                    }
                } else if bchars.next().is_some() {
                    return Ordering::Less;
                } else {
                    // Don't return Equal, still make it
                    // deterministic:
                    return stricter_ordering.then_with(|| a.cmp(b));
                }
            }
        }
    }
    a.cmp(b)
}
