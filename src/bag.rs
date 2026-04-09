//! A tree data structure for efficient collection of items from
//! recursive algorithms
//!
//! Allowing variable depth without copying, only flattening at the
//! end.

// Also see: `StringTree` (xmlhub-indexer)

use std::{
    mem::{swap, MaybeUninit},
    num::NonZeroUsize,
};

use arbitrary::Arbitrary;

use crate::{hack_static::hack_static, probe};

#[derive(Debug, Clone)]
pub enum Bag<T> {
    Empty,
    Leaf(T),
    LeafVec(Vec<T>),
    Branching(NonZeroUsize, Vec<Bag<T>>),
}

#[test]
fn t_bag_size() {
    use std::mem::size_of;
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
        let len: usize = value.iter().map(|b| b.len()).sum();
        if len == 0 {
            Bag::Empty
        } else {
            Bag::Branching(
                NonZeroUsize::new(len).expect("checked not 0"),
                value,
            )
        }
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
            Bag::Branching(_, _) => false,
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Bag::Empty => 0,
            Bag::Leaf(_) => 1,
            Bag::LeafVec(items) => items.len(),
            Bag::Branching(len, _) => (*len).into(),
        }
    }

    pub fn add_bag(self, val: Bag<T>) -> Bag<T> {
        if val.is_empty() {
            self
        } else if self.is_empty() {
            val
        } else {
            match val {
                Bag::Empty => unreachable!(),
                Bag::Leaf(item) => match self {
                    Bag::Empty => Bag::Leaf(item),
                    Bag::Leaf(item0) => Bag::LeafVec(vec![item0, item]),
                    Bag::LeafVec(mut items) => {
                        items.push(item);
                        Bag::LeafVec(items)
                    }
                    Bag::Branching(len, mut bags) => {
                        bags.push(Bag::Leaf(item));
                        let len = usize::from(len) + 1;
                        Bag::Branching(
                            NonZeroUsize::new(len).expect("+1 above"),
                            bags,
                        )
                    }
                },
                Bag::LeafVec(mut items) => {
                    match self {
                        Bag::Empty => Bag::LeafVec(items),
                        Bag::Leaf(item) => {
                            // hmm, if ordering does not need
                            // maintainance, could push `item` to the end. But?
                            let len = items.len() + 1;
                            Bag::Branching(
                                NonZeroUsize::new(len).expect("+1 above"),
                                vec![Bag::Leaf(item), Bag::LeafVec(items)],
                            )
                        }
                        Bag::LeafVec(mut items0) => {
                            if items.len() < 1500 {
                                items0.append(&mut items);
                                Bag::LeafVec(items0)
                            } else {
                                // Delay large move until the end.
                                let len = items.len() + items0.len();
                                Bag::Branching(
                                    NonZeroUsize::new(len)
                                        .expect("both checked to be non-empty"),
                                    vec![
                                        Bag::LeafVec(items0),
                                        Bag::LeafVec(items),
                                    ],
                                )
                            }
                        }
                        Bag::Branching(len, mut bags) => {
                            let len = usize::from(len) + items.len();
                            bags.push(Bag::LeafVec(items));
                            Bag::Branching(
                                NonZeroUsize::new(len)
                                    .expect("was already branching len"),
                                bags,
                            )
                        }
                    }
                }
                Bag::Branching(len, bags) => {
                    if self.is_empty() {
                        Bag::Branching(len, bags)
                    } else {
                        let tot_len = self.len() + usize::from(len);
                        Bag::Branching(
                            NonZeroUsize::new(tot_len)
                                .expect("was already branching len"),
                            vec![self, Bag::Branching(len, bags)],
                        )
                    }
                }
            }
        }
    }

    pub fn push_bag(&mut self, val: Bag<T>) {
        let mut this = Bag::Empty;
        swap(&mut this, self);
        *self = this.add_bag(val);
    }

    pub fn push(&mut self, val: T) {
        // XX should we optimize by having our own specialized
        // dispatch here? But, just make sure push_bag/add_bag is
        // inlined?
        self.push_bag(Bag::Leaf(val));
    }

    fn _flatten(self, out: &mut Vec<T>) {
        match self {
            Bag::Empty => (),
            Bag::Leaf(item) => out.push(item),
            Bag::LeafVec(mut items) => out.append(&mut items),
            Bag::Branching(_, bags) => {
                for bag in bags {
                    bag._flatten(out);
                }
            }
        }
    }

    pub fn flatten(self) -> Vec<T> {
        let len = self.len();
        let mut out = Vec::with_capacity(len);
        self._flatten(&mut out);
        out
    }

    pub fn par_flatten(self) -> Vec<T>
    where
        T: Send,
    {
        let len = self.len();
        if len < 500000 {
            self.flatten()
        } else {
            match self {
                Bag::Empty => Vec::new(),
                Bag::Leaf(item) => {
                    probe!("par_flatten Leaf");
                    vec![item]
                }
                Bag::LeafVec(items) => {
                    probe!("par_flatten LeafVec");
                    items
                }
                Bag::Branching(_, mut bags) => {
                    probe!("par_flatten Branching");
                    let mut out: Vec<MaybeUninit<T>> = Vec::with_capacity(len);
                    unsafe {
                        // Safe because it is MaybeUninit, and we set
                        // the source to Bag::Empty before converting
                        // to initialized (we might leak, but won't
                        // double free)
                        out.set_len(len)
                    };
                    let out_rf = &mut out;
                    rayon::scope(|scope| {
                        let bags_len = bags.len();
                        let mut n = 0;
                        let mut last_n_spawned = 0;
                        let mut last_i_spawned = 0;
                        for i in 0..bags_len {
                            n += bags[i].len();
                            let out_len = n - last_n_spawned;
                            if out_len > 500000 {
                                let i1 = i + 1;
                                probe!(format!(
                                    "will spawn _par_flatten bags[{last_i_spawned}..{i1}], \
                                     out[{last_n_spawned}..{n}]"));
                                let bagsrf = unsafe {
                                    hack_static(&mut bags[last_i_spawned..i1])
                                };
                                let outrf = unsafe {
                                    hack_static(&mut out_rf[last_n_spawned..n])
                                };
                                scope.spawn(|_| {
                                    _par_flatten(bagsrf, outrf);
                                });
                                last_i_spawned = i1;
                                last_n_spawned = n;
                            }
                        }
                        if last_i_spawned < bags.len() {
                            _par_flatten(
                                &mut bags[last_i_spawned..bags_len],
                                &mut out_rf[last_n_spawned..len],
                            );
                        }
                    });
                    unsafe {
                        // MaybeUninit::assume_init(out)
                        out.into_iter()
                            .map(|v| MaybeUninit::assume_init(v))
                            .collect()
                    }
                }
            }
        }
    }
}

fn _par_flatten<T: Send>(
    from: &mut [Bag<T>],
    to: &mut [MaybeUninit<T>],
) -> usize {
    // probe!(format!("_par_flatten from.len = {}, to.len= {}", from.len(), to.len()));
    let mut to_i = 0;
    for frombag in from {
        let mut bag = Bag::Empty;
        swap(&mut bag, frombag);
        match bag {
            Bag::Empty => (),
            Bag::Leaf(item) => {
                to[to_i] = MaybeUninit::new(item);
                to_i += 1;
            }
            Bag::LeafVec(items) => {
                for item in items {
                    to[to_i] = MaybeUninit::new(item);
                    to_i += 1;
                }
            }
            Bag::Branching(_, mut bags) => {
                to_i += _par_flatten(&mut bags, &mut to[to_i..]);
            }
        }
    }
    to_i
}

// Somehow derive(Arbitrary) always yielded Empty. As does
// Vec::arbitrary. So implement it explicitly and avoid normal
// arbitrary impls, huh. Also, we need to taper off to get some decent
// tree sizes, thus use our own API and then wrap that with Arbitrary.

fn my_arbitrary_vec<'a, T: Arbitrary<'a>>(
    u: &mut arbitrary::Unstructured<'a>,
    depth: usize,
) -> arbitrary::Result<(usize, Vec<Bag<T>>)> {
    let len = u.int_in_range(1..=50)?;
    let mut vec = Vec::new();
    for _ in 0..len {
        vec.push(my_arbitrary_bag(u, depth + 1)?);
    }
    let size = vec.iter().map(|b| b.len()).sum();
    Ok((size, vec))
}

fn my_arbitrary_bag<'a, T: Arbitrary<'a>>(
    u: &mut arbitrary::Unstructured<'a>,
    depth: usize,
) -> arbitrary::Result<Bag<T>> {
    match (|| -> arbitrary::Result<_> {
        let n = u.int_in_range(0..=depth)?;
        if n == 0 {
            let (size, vec) = my_arbitrary_vec(u, depth + 1)?;
            if size == 0 {
                Ok(Bag::Empty)
            } else {
                Ok(Bag::Branching(size.try_into().expect("checked"), vec))
            }
        } else {
            match u.int_in_range(0..=10)? {
                0..=5 => Ok(Bag::LeafVec(Vec::arbitrary(u)?)),
                6..=8 => Ok(Bag::Empty),
                9..=10 => Ok(Bag::Leaf(T::arbitrary(u)?)),
                _ => unreachable!(),
            }
        }
    })() {
        Ok(v) => Ok(v),
        Err(_) => {
            eprintln!("fallback");
            Ok(Bag::Empty)
        }
    }
}

impl<'a, T: Arbitrary<'a>> Arbitrary<'a> for Bag<T> {
    fn arbitrary(
        u: &mut arbitrary::Unstructured<'a>,
    ) -> arbitrary::Result<Self> {
        my_arbitrary_bag(u, 0)
    }
}

#[cfg(test)]
mod tests {
    use anyhow::Result;
    use arbitrary::{Arbitrary, Unstructured};
    use getrandom::getrandom;

    use super::*;

    #[test]
    fn t_() -> Result<()> {
        let mut random_data = vec![0; 5_000_000];
        getrandom(&mut random_data)?;
        let mut data = Unstructured::new(&random_data);
        let bag = Bag::<i32>::arbitrary(&mut data)?;

        let show_it = false;
        if show_it {
            eprintln!("{bag:?}");
        }

        let l1 = bag.clone().flatten();
        let l2 = bag.par_flatten();
        assert_eq!(l1, l2);

        if show_it {
            panic!();
        }

        Ok(())
    }
}
