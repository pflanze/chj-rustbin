/// Combine two instances of the same type into one
pub trait Merge: Sized {
    fn mut_merge(&mut self, other: Self);

    fn merge(mut self, other: Self) -> Self {
        self.mut_merge(other);
        self
    }
}
