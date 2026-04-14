//! A tree data structure for efficient collection of items from
//! recursive algorithms
//!
//! Allowing variable depth without copying, only flattening at the
//! end.

// Also see: `StringTree` (xmlhub-indexer)

use std::{
    mem::{swap, transmute, MaybeUninit},
    num::NonZeroUsize,
};

use arbitrary::Arbitrary;
use log::debug;

use crate::{probe, unsafe_util::unsafe_sync_send::UnsafeSync};

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
        // Should we optimize by having our own specialized
        // dispatch here?
        match self {
            Bag::Empty => {
                *self = Bag::Leaf(val);
                return;
            }
            Bag::Leaf(_) => (),
            Bag::LeafVec(ref mut items) => {
                items.push(val);
                return;
            }
            Bag::Branching(_, _) => (),
        }
        // Fallback:
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

    pub fn par_flatten(self, min_out_slice_len: usize) -> Vec<T>
    where
        T: Send,
    {
        let len = self.len();
        if len < min_out_slice_len {
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
                Bag::Branching(_, bags) => {
                    probe!(format!(
                        "par_flatten Branching with bags lengths = {:?}",
                        bags.iter().map(|b| b.len()).collect::<Vec<_>>()
                    ));
                    let bags = uninit_vec(bags);
                    let mut out: Vec<MaybeUninit<UnsafeSync<T>>> =
                        Vec::with_capacity(len);
                    unsafe {
                        // Safe because the elements are MaybeUninit,
                        // and when moving out of the source to this,
                        // we set the source to Bag::Empty before
                        // converting this Vec to initialized (we
                        // might leak, but won't double free)
                        out.set_len(len)
                    };
                    let mut bags_rest = &*bags;
                    let mut out_rest = &mut *out;
                    rayon::scope(move |scope| {
                        // Go through all bags, increase the output
                        // slice length until it is at least
                        // MIN_OUT_SLICE_LEN long, then spawn a task
                        // for copying it.
                        let mut bags_rest_i = 0;
                        // The number of entries collected to be
                        // copied into `out` at any time
                        let mut n = 0;
                        // The remaining number of entries to do, to
                        // judge if a spawn is really warranted
                        let mut n_rest = len;
                        while bags_rest_i < bags_rest.len() {
                            dbg!(n_rest);
                            {
                                let bag_len = unsafe {
                                    let b =
                                        &*(&bags_rest[bags_rest_i]).as_ptr();
                                    b.deref()
                                }
                                .len();
                                n += bag_len;
                                n_rest -= bag_len;
                            }
                            let bags_rest_i1 = bags_rest_i + 1;
                            if n >= min_out_slice_len {
                                if n_rest < min_out_slice_len / 2 {
                                    debug!(
                                        "do not spawn, instead do the whole \
                                         rest ({n_rest}) as the last chunk"
                                    );
                                    break;
                                }
                                let bagsrf;
                                (bagsrf, bags_rest) =
                                    bags_rest.split_at(bags_rest_i1);
                                let outrf;
                                (outrf, out_rest) = out_rest.split_at_mut(n);
                                probe!(format!("_par_flatten for n={n}"));
                                scope.spawn(move |_| {
                                    _par_flatten(bagsrf, outrf);
                                });
                                (bags_rest_i, n) = (0, 0);
                            } else {
                                bags_rest_i = bags_rest_i1;
                            }
                        }
                        // The last chunk smaller than the checked
                        // MIN_OUT_SLICE_LEN above is processed
                        // without spawning a new task
                        if n > 0 {
                            probe!(format!("_par_flatten remainder n={n} out_rest.len()={}",
                                           out_rest.len()));
                            _par_flatten(bags_rest, out_rest);
                        }
                    });
                    // Enforce the assumption that `bags` exists past
                    // the rayon scope.
                    drop(bags);
                    {
                        probe!("assume_init");
                        unsafe {
                            // out.into_iter()
                            //     .map(|v| MaybeUninit::assume_init(v))
                            //     .collect()
                            transmute(out)
                        }
                    }
                }
            }
        }
    }
}

/// Safety: if used to memcpy items, then `slice` must be inside a
/// MaybeUninit wrapper.
unsafe fn uninit_slice<T>(slice: &[T]) -> &[MaybeUninit<UnsafeSync<T>>] {
    unsafe { transmute(slice) }
}

fn uninit_vec<T>(vec: Vec<T>) -> Vec<MaybeUninit<UnsafeSync<T>>> {
    // Safe because the two T wrappers are transparent, prevent access
    // without unsafe (hence another thread can't access it anyway
    // safely), and the values are now ManuallyDrop, hence while leaks
    // are possible, those are not unsafe.
    unsafe { transmute(vec) }
}

/// Only safe if &T is inside a MaybeUninit wrapper, since a memcpy is
/// made without marking T as now being invalid.
unsafe fn copy_item<T>(from: &T, to: &mut MaybeUninit<UnsafeSync<T>>) {
    unsafe {
        let to = &mut *to.as_mut_ptr();
        let to = to.deref_mut();
        let to: *mut T = to;
        std::ptr::copy_nonoverlapping(from, to, 1);
    }
}

/// Safety: `from` is invalidated, hence must not be aliasing with
/// storage that is *not* MaybeUninit. `to` must be at least as long
/// as `from` (currently checked, panics otherwise).
unsafe fn copy_slice<T>(
    from: &[MaybeUninit<UnsafeSync<T>>],
    to: &mut [MaybeUninit<UnsafeSync<T>>],
) -> usize {
    let len = from.len();
    let to_ptr = to.as_mut_ptr();
    assert!(len <= to.len());
    unsafe {
        std::ptr::copy_nonoverlapping(from.as_ptr(), to_ptr, len);
    }
    len
}

fn _par_flatten<T: Send>(
    from: &[MaybeUninit<UnsafeSync<Bag<T>>>],
    to: &mut [MaybeUninit<UnsafeSync<T>>],
) -> usize {
    let mut to_i = 0;
    for frombag in from {
        let frombag = unsafe { &*frombag.as_ptr() };
        let frombag = unsafe { frombag.deref() };
        match frombag {
            Bag::Empty => (),
            Bag::Leaf(item) => {
                unsafe { copy_item(item, &mut to[to_i]) };
                to_i += 1;
            }
            Bag::LeafVec(items) => {
                to_i +=
                    unsafe { copy_slice(uninit_slice(items), &mut to[to_i..]) };
            }
            Bag::Branching(_, bags) => {
                to_i += _par_flatten(
                    unsafe {
                        // Safe because bags is within a MaybeUninit
                        // wrapper
                        uninit_slice(&bags)
                    },
                    &mut to[to_i..],
                );
            }
        }
    }
    to_i
}

// -----------------------------------------------------------------------------
// Testing

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
        let n = u.int_in_range(0..=(depth / 2))?;
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
            debug!("fallback");
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
