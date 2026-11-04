//! Chunks of Result values, ending early in case of Err
//!
//! Adapted copy of `chunks.rs`

pub trait TryChunksOp<T, E>: Iterator<Item = Result<T, E>> {
    fn try_chunks(self, max_items: usize) -> TryChunks<T, E, Self>
    where
        Self: Sized;
}

impl<T, E, I: Iterator<Item = Result<T, E>>> TryChunksOp<T, E> for I {
    fn try_chunks(self, max_items: usize) -> TryChunks<T, E, Self>
    where
        Self: Sized,
    {
        TryChunks {
            max_items,
            iter: self,
            exhausted: false,
        }
    }
}

pub struct TryChunks<T, E, I: Iterator<Item = Result<T, E>>> {
    max_items: usize,
    iter: I,
    exhausted: bool,
}

impl<T, E, I: Iterator<Item = Result<T, E>>> Iterator for TryChunks<T, E, I> {
    type Item = Vec<Result<T, E>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.exhausted {
            return None;
        }
        let mut vs = Vec::with_capacity(self.max_items);
        loop {
            if let Some(v) = self.iter.next() {
                let end = v.is_err();
                vs.push(v);
                if end {
                    break;
                }
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
