//! Make a `Vec` sorted by its `len` first
//!
//! Useful to maintain a collection of `Vec` sorted by length in a
//! `BTreeSet`.

#[derive(Debug, PartialEq, Eq)]
pub struct VecByLen<T: PartialOrd>(pub Vec<T>);

impl<T: Ord> Ord for VecByLen<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0
            .len()
            .cmp(&other.0.len())
            .then_with(|| self.0.cmp(&other.0))
    }
}

impl<T: PartialOrd> PartialOrd for VecByLen<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.0.len().cmp(&other.0.len()) {
            std::cmp::Ordering::Equal => self.0.partial_cmp(&other.0),
            v => Some(v),
        }
    }
}

impl<T: PartialOrd> From<Vec<T>> for VecByLen<T> {
    fn from(value: Vec<T>) -> Self {
        Self(value)
    }
}
