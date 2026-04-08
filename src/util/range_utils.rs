use std::ops::RangeInclusive;

pub trait RangeUtils {
    fn range_overlaps(&self, other: &Self) -> bool;
}

impl<T: PartialOrd> RangeUtils for RangeInclusive<T> {
    fn range_overlaps(&self, other: &Self) -> bool {
        self.contains(other.start())
            || self.contains(other.end())
            || other.contains(self.start())
            || other.contains(self.end())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn t(a: RangeInclusive<u16>, b: RangeInclusive<u16>, r: bool) {
        let actual = a.range_overlaps(&b);
        if actual != r {
            panic!("{a:?} .range_overlaps( {b:?} ) yields {actual}, but expected {r}")
        }
    }

    #[test]
    fn t_() {
        t(0..=1, 0..=1, true);
        t(0..=1, 1..=2, true);
        t(0..=1, 2..=3, false);
        t(0..=10, 2..=3, true);
        t(2..=3, 0..=10, true);
        t(2..=10, 0..=20, true);
        t(2..=10, 0..=4, true);
        t(2..=10, 4..=12, true);
    }
}
