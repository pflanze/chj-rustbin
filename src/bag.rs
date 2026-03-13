//! A tree data structure for efficient collection of items from
//! recursive algorithms
//!
//! Allowing variable depth without copying, only flattening at the
//! end.

// Also see: `StringTree` (xmlhub-indexer)

use std::mem::swap;

#[derive(Debug)]
pub enum Bag<T> {
    Empty,
    Leaf(T),
    LeafVec(Vec<T>),
    Branching(Vec<Bag<T>>),
}

#[test]
fn t_bag_size() {
    assert_eq!(size_of::<Bag<bool>>(), 8 * 4);
}

impl<T> Default for Bag<T> {
    fn default() -> Self {
        Bag::Empty
    }
}

impl<T> From<T> for Bag<T> {
    fn from(value: T) -> Self {
        Bag::Leaf(value)
    }
}

impl<T> From<Vec<T>> for Bag<T> {
    fn from(value: Vec<T>) -> Self {
        Bag::LeafVec(value)
    }
}

impl<T> From<Vec<Bag<T>>> for Bag<T> {
    fn from(value: Vec<Bag<T>>) -> Self {
        Bag::Branching(value)
    }
}

impl<T> Bag<T> {
    pub fn new() -> Self {
        Bag::Empty
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Bag::Empty => true,
            Bag::Leaf(_) => false,
            Bag::LeafVec(items) => items.is_empty(),
            // XX just assume that a bag is never built with a
            // Branching but no non-empty bags
            Bag::Branching(bags) => bags.is_empty(),
        }
    }

    pub fn add_bag(self, val: Bag<T>) -> Bag<T> {
        match val {
            Bag::Empty => self,
            Bag::Leaf(item) => match self {
                Bag::Empty => Bag::Leaf(item),
                Bag::Leaf(item0) => Bag::LeafVec(vec![item0, item]),
                Bag::LeafVec(mut items) => {
                    items.push(item);
                    Bag::LeafVec(items)
                }
                Bag::Branching(mut bags) => {
                    bags.push(Bag::Leaf(item));
                    Bag::Branching(bags)
                }
            },
            Bag::LeafVec(mut items) => {
                match self {
                    Bag::Empty => Bag::LeafVec(items),
                    Bag::Leaf(item) => {
                        // hmm, if ordering does not need
                        // maintainance, could push `item` to the end. But?
                        Bag::Branching(vec![
                            Bag::Leaf(item),
                            Bag::LeafVec(items),
                        ])
                    }
                    Bag::LeafVec(mut items0) => {
                        items0.append(&mut items);
                        Bag::LeafVec(items0)
                    }
                    Bag::Branching(mut bags) => {
                        bags.push(Bag::LeafVec(items));
                        Bag::Branching(bags)
                    }
                }
            }
            Bag::Branching(bags) => {
                if self.is_empty() {
                    Bag::Branching(bags)
                } else {
                    Bag::Branching(vec![self, Bag::Branching(bags)])
                }
            }
        }
    }

    pub fn push_bag(&mut self, val: Bag<T>) {
        let mut this = Bag::Empty;
        swap(&mut this, self);
        *self = this.add_bag(val);
    }

    pub fn _flatten(self, out: &mut Vec<T>) {
        match self {
            Bag::Empty => (),
            Bag::Leaf(item) => out.push(item),
            Bag::LeafVec(mut items) => out.append(&mut items),
            Bag::Branching(bags) => {
                for bag in bags {
                    bag._flatten(out);
                }
            }
        }
    }

    pub fn flatten(self) -> Vec<T> {
        let mut out = Vec::new();
        self._flatten(&mut out);
        out
    }
}

// XXX tests
