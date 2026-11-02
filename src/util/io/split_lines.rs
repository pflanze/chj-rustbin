//! Split bytes on line terminators or record separators

use rayon::{iter::ParallelIterator, slice::ParallelSlice};
use std::iter::FusedIterator;

pub trait SplitLines {
    /// Does not report an empty line after the last
    /// `record_separator`. Does not report an error if the last line
    /// doesn't end with `record_separator`, either.
    fn split_lines<'s>(
        &'s self,
        record_separator: u8,
    ) -> impl Iterator<Item = &'s [u8]> + DoubleEndedIterator + FusedIterator + 's;

    /// Does not report an empty line after the last
    /// `record_separator`. Does not report an error if the last line
    /// doesn't end with `record_separator`, either.
    fn par_split_lines<'s>(
        &'s self,
        record_separator: u8,
    ) -> impl ParallelIterator<Item = &'s [u8]> + 's;
}

impl SplitLines for [u8] {
    fn split_lines<'s>(
        &'s self,
        record_separator: u8,
    ) -> impl Iterator<Item = &'s [u8]> + DoubleEndedIterator + FusedIterator + 's
    {
        let without_trailing_separator =
            if self.last() == Some(&record_separator) {
                &self[0..self.len() - 1]
            } else {
                self
            };
        without_trailing_separator.split(move |b| *b == record_separator)
    }

    fn par_split_lines<'s>(
        &'s self,
        record_separator: u8,
    ) -> impl ParallelIterator<Item = &'s [u8]> + 's {
        let without_trailing_separator =
            if self.last() == Some(&record_separator) {
                &self[0..self.len() - 1]
            } else {
                self
            };
        without_trailing_separator.par_split(move |b| *b == record_separator)
    }
}
