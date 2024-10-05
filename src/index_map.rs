//! A Vec that automatically grows to accommodate the index passed to
//! `insert`, and tracks occupancy of its slots.

// Also offer a variant that requires T to implement Default?

use std::{mem::replace, ops::{Index, IndexMut}};


#[derive(Debug)]
pub struct IndexMap<T>(Vec<Option<T>>);

impl<T> IndexMap<T> {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn insert(&mut self, key: usize, val: T) -> Option<T> {
        let oldlen = self.0.len();
        if key >= oldlen {
            if key > oldlen {
                self.0.resize_with(key, || None);
            }
            self.0.push(Some(val));
            None
        } else {
            replace(&mut self.0[key], Some(val))
        }
    }

    pub fn get(&self, key: usize) -> Option<&T> {
        if key < self.0.len() {
            self.0[key].as_ref()
        } else {
            None
        }
    }

    pub fn get_mut(&mut self, key: usize) -> Option<&mut T> {
        if key < self.0.len() {
            self.0[key].as_mut()
        } else {
            None
        }
    }
}

impl<T> Index<usize> for IndexMap<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).expect("index within bounds")
    }
}

impl<T> IndexMut<usize> for IndexMap<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.get_mut(index).expect("index within bounds")
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_general() {
        let mut m = IndexMap::new();
        assert_eq!(m.insert(10, 10), None);
        assert_eq!(m.insert(10, 101), Some(10));
        assert_eq!(m.insert(11, 11), None);
        assert_eq!(m.insert(0, 1000), None);
        assert_eq!(m.insert(0, 100), Some(1000));
        assert_eq!(m.insert(12, 12), None);

        assert_eq!(m.get(0), Some(&100));
        assert_eq!(m.get(1), None);
        assert_eq!(m.get(10), Some(&101));
        assert_eq!(m.get(11), Some(&11));
        assert_eq!(m.get(12), Some(&12));
        assert_eq!(m.get(13), None);

        assert_eq!(&m[0], &100);
        assert_eq!(m[10], 101);

        m[10] = 10101;
        
        assert_eq!(m.get(10), Some(&10101));
    }

    #[test]
    #[should_panic]
    fn t_inaccessible() {
        let m = IndexMap::<u32>::new();
        m[0];
    }

    #[test]
    #[should_panic]
    fn t_inaccessible_1() {
        let mut m = IndexMap::new();
        m.insert(10, 10);
        m[3];
    }
}
