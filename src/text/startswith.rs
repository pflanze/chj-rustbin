pub trait StartsWith<I> {
    fn starts_with(&mut self, start: &mut I) -> Option<usize>;
}

impl<T: Eq, I: Iterator<Item = T>> StartsWith<I> for I {
    fn starts_with(&mut self, start: &mut I) -> Option<usize> {
        let mut len: usize = 0;
        loop {
            if let Some(b) = start.next() {
                if let Some(a) = self.next() {
                    if a != b {
                        return None;
                    } else {
                        len += 1;
                    }
                } else {
                    return None;
                }
            } else {
                return Some(len);
            }
        }
    }
}
