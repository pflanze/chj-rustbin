//! Slicing, reversing and filtering a sequence with the least amount
//! of copying possible.

use std::ops::{Range, RangeFrom};

/// Like taking a sub-slice, but on a range. (Why does `Range` not
/// implement Index by Range? Deemed too confusing?)
fn subrange_of_range(
    range: Range<usize>,
    subrange: Range<usize>,
) -> Option<Range<usize>> {
    let Range {
        start: origstart,
        end: origend,
    } = range;
    let Range {
        start: substart,
        end: subend,
    } = subrange;
    let start = origstart.checked_add(substart)?;
    let sublen = subend.checked_sub(substart)?;
    let end = start.checked_add(sublen)?;
    if end > origend {
        return None;
    }
    Some(Range { start, end })
}

#[test]
fn t_subrange_of_range() {
    let t = subrange_of_range;
    assert_eq!(t(0..100, 0..10), Some(0..10));
    assert_eq!(t(0..100, 0..100), Some(0..100));
    assert_eq!(t(0..100, 0..101), None);

    assert_eq!(t(20..100, 0..10), Some(20..30));
    assert_eq!(t(20..100, 0..80), Some(20..100));
    assert_eq!(t(20..100, 0..81), None);

    assert_eq!(t(0..100, 10..10), Some(10..10));
    assert_eq!(t(0..100, 10..20), Some(10..20));
    assert_eq!(t(0..100, 10..100), Some(10..100));
    assert_eq!(t(0..100, 10..101), None);

    assert_eq!(t(20..100, 10..10), Some(30..30));
    assert_eq!(t(20..100, 10..20), Some(30..40));
    assert_eq!(t(20..100, 10..80), Some(30..100));
    assert_eq!(t(20..100, 10..81), None);
}

#[derive(Debug)]
pub enum FilteredSlice<'vals, 's, T> {
    Owned(&'s [T]),
    Refs(&'s [&'vals T]),
}

impl<'vals, 's, T> FilteredSlice<'vals, 's, T> {
    /// Allocates the result; only meant for debugging.
    pub fn to_ref_vec(self) -> Vec<&'s T> {
        match self {
            FilteredSlice::Owned(items) => items.iter().collect(),
            FilteredSlice::Refs(items) => items.to_vec(),
        }
    }

    /// Allocates the result and clones all items; only meant for
    /// debugging.
    pub fn to_owning_vec(self) -> Vec<T>
    where
        T: Clone,
    {
        match self {
            FilteredSlice::Owned(items) => items.iter().cloned().collect(),
            FilteredSlice::Refs(items) => {
                items.iter().map(|r| (*r).clone()).collect()
            }
        }
    }
}

impl<'vals, 's, T: PartialEq> PartialEq for FilteredSlice<'vals, 's, T> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Owned(l), Self::Owned(r)) => l == r,
            (Self::Refs(l), Self::Refs(r)) => l == r,
            (Self::Owned(l), Self::Refs(r)) => {
                l.iter().zip(r.iter()).all(|(a, b)| a == *b)
            }
            (Self::Refs(l), Self::Owned(r)) => {
                l.iter().zip(r.iter()).all(|(a, b)| *a == b)
            }
        }
    }
}

#[derive(Debug)]
pub enum Filtered<'vals, T> {
    RefOwned(&'vals mut [T]),
    OwnedRefs {
        refs: Vec<&'vals T>,
        range_of_refs: Range<usize>,
    },
}

impl<'vals, T: PartialEq> PartialEq for Filtered<'vals, T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl<'vals, T> Filtered<'vals, T> {
    pub fn as_slice<'s>(&'s self) -> FilteredSlice<'vals, 's, T>
    where
        'vals: 's,
    {
        match self {
            Filtered::RefOwned(items) => FilteredSlice::Owned(items),
            Filtered::OwnedRefs {
                refs,
                range_of_refs,
            } => FilteredSlice::Refs(&refs[range_of_refs.clone()]),
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Filtered::RefOwned(items) => items.len(),
            Filtered::OwnedRefs {
                refs: _,
                range_of_refs,
            } => range_of_refs.len(),
        }
    }

    pub fn reverse(&mut self) {
        match self {
            Filtered::RefOwned(items) => items.reverse(),
            Filtered::OwnedRefs {
                refs,
                range_of_refs,
            } => refs[range_of_refs.clone()].reverse(),
        }
    }

    pub fn filter(self, mut pred: impl FnMut(&T) -> bool) -> Self {
        match self {
            Filtered::RefOwned(items) => {
                let refs: Vec<&T> = items.iter().filter(|v| pred(*v)).collect();
                let range_of_refs = 0..refs.len();
                Filtered::OwnedRefs {
                    refs,
                    range_of_refs,
                }
            }
            Filtered::OwnedRefs {
                refs,
                range_of_refs,
            } => {
                let refs: Vec<&T> = refs[range_of_refs]
                    .iter()
                    .filter(|v| pred(*v))
                    .map(|r| *r)
                    .collect();
                let range_of_refs = 0..refs.len();
                Filtered::OwnedRefs {
                    refs,
                    range_of_refs,
                }
            }
        }
    }

    pub fn slice_from(self, range: RangeFrom<usize>) -> Self {
        let RangeFrom { start } = range;
        let len = self.len();
        self.slice(start..len)
    }

    pub fn slice(self, range: Range<usize>) -> Self {
        match self {
            Filtered::RefOwned(items) => Filtered::RefOwned(&mut items[range]),
            Filtered::OwnedRefs {
                refs,
                range_of_refs,
            } => Filtered::OwnedRefs {
                refs,
                range_of_refs: subrange_of_range(range_of_refs, range)
                    .expect("expect range within len of Filtered"),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_owning_vec() {
        let mut v = vec![1, 30, 4];
        let a0 = Filtered::RefOwned(&mut v);
        let a = a0.as_slice();
        matches!(a, FilteredSlice::Owned(_));
        let a = a.to_owning_vec();
        let b0 = Filtered::RefOwned(&mut v).filter(|n| *n < 100);
        let b = b0.as_slice();
        matches!(b, FilteredSlice::Refs(_));
        let b = b.to_owning_vec();
        assert_eq!(a, b);
    }

    #[test]
    fn t_filtered_slice_eq() {
        let mut v1 = vec![1, 30, 4];
        let mut v2 = vec![1, 30, 300, 4];
        let a0 = Filtered::RefOwned(&mut v1);
        let a = a0.as_slice();
        matches!(a, FilteredSlice::Owned(_));
        let b0 = Filtered::RefOwned(&mut v2).filter(|n| *n < 100);
        let b = b0.as_slice();
        matches!(b, FilteredSlice::Refs(_));
        assert_eq!(a, b);
    }
}
