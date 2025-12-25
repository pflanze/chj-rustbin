//! Somehow chunks from itertools 0.11.0 is not Send, thus try my own.

pub trait ChunksOp<T>: Iterator<Item = T> {
    fn chunks(self, max_items: usize) -> Chunks<T, Self>
    where
        Self: Sized;
}

pub struct Chunks<T, I: Iterator<Item = T>> {
    max_items: usize,
    iter: I,
    exhausted: bool,
}

impl<T, I: Iterator<Item = T>> Iterator for Chunks<T, I> {
    type Item = Vec<T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.exhausted {
            return None;
        }
        let mut vs = Vec::with_capacity(self.max_items);
        loop {
            if let Some(v) = self.iter.next() {
                vs.push(v);
            } else {
                self.exhausted = true;
                break;
            }
            if vs.len() >= self.max_items {
                break;
            }
        }
        if vs.is_empty() {
            None
        } else {
            Some(vs)
        }
    }
}

impl<T, I: Iterator<Item = T>> ChunksOp<T> for I {
    fn chunks(self, max_items: usize) -> Chunks<T, Self>
    where
        Self: Sized,
    {
        Chunks {
            max_items,
            iter: self,
            exhausted: false,
        }
    }
}
