//! Cons lists
//!
//! These only work with normal references. That's because making it
//! generic for the reference/container type appears unworkable, even
//! though feasible in principle, due to requiring explicit type
//! parameters on every single cons call. The solution would be to
//! generate the code (via macros) for versions using Rc, Arc or
//! whatever when needed; but there are more performant
//! representations for persistent lists than cons lists if they need
//! to pay for the heap allocation overhead, so the reference case may
//! be the only interesting one.
//!
//! For a usage example, see `find_in_tree` in the unit tests.

pub enum List<'t, T> {
    Pair(T, &'t List<'t, T>),
    Null,
}

pub fn cons<'l, T>(v: T, r: &'l List<T>) -> List<'l, T> {
    List::Pair(v, r)
}

impl<'t, T> List<'t, T> {
    pub fn is_empty(&self) -> bool {
        match self {
            List::Pair(_, _) => false,
            List::Null => true,
        }
    }

    pub fn len(&self) -> usize {
        match self {
            List::Pair(_, r) => r.len() + 1,
            List::Null => 0,
        }
    }

    pub fn first(&self) -> Option<&T> {
        match self {
            List::Pair(v, _) => Some(v),
            List::Null => None,
        }
    }

    pub fn rest(&self) -> Option<&List<'_, T>> {
        match self {
            List::Pair(_, r) => Some(r),
            List::Null => None,
        }
    }

    pub fn last(&self) -> Option<&T> {
        match self {
            List::Pair(v, List::Null) => Some(v),
            List::Pair(_, r) => r.last(),
            List::Null => None,
        }
    }

    /// A Vec of all the values as references.
    // For reverse simply reverse the Vec afterwards yourself? (Could
    // also get length then fill in unsafely or require Default.)
    pub fn as_ref_vec(&self) -> Vec<&T> {
        let mut vs = Vec::new();
        let mut r = self;
        #[allow(clippy::while_let_loop)]
        while let List::Pair(v, r2) = r {
            vs.push(v);
            r = r2;
        }
        vs
    }

    pub fn to_vec(&self) -> Vec<T>
    where
        T: Clone,
    {
        let mut vs: Vec<T> = Vec::new();
        let mut r = self;
        #[allow(clippy::while_let_loop)]
        while let List::Pair(v, r2) = r {
            vs.push(v.clone());
            r = r2;
        }
        vs
    }

    pub fn any(&self, mut f: impl FnMut(&T) -> bool) -> bool {
        let mut vs = self;
        loop {
            match vs {
                List::Pair(val, list) => {
                    if f(val) {
                        return true;
                    }
                    vs = list;
                }
                List::Null => return false,
            }
        }
    }
}

impl<'t, T: PartialEq> List<'t, T> {
    /// Report whether a List contains a particular value.
    pub fn contains(&self, val: &T) -> bool {
        let mut l = self;
        loop {
            match l {
                List::Pair(v, rest) => {
                    if v == val {
                        return true;
                    } else {
                        l = rest;
                    }
                }
                List::Null => return false,
            }
        }
    }
}

impl<'t, K: PartialEq, V> List<'t, (K, V)> {
    /// In a List of (K, V) pairs, get the first V for which the K ==
    /// key.
    pub fn alist_get(&self, key: &K) -> Option<&V> {
        let mut l = self;
        loop {
            match l {
                List::Pair((k, v), rest) => {
                    if k == key {
                        return Some(v);
                    } else {
                        l = rest;
                    }
                }
                List::Null => return None,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_general() {
        let a = List::Pair(5, &List::Null);
        let b = List::Pair(7, &a);
        let c = List::Pair(9, &b);
        let d = cons(13, &b);
        let e = cons(14, &c);
        assert_eq!(List::Null::<i8>.as_ref_vec(), Vec::<&i8>::new());
        assert_eq!(a.to_vec(), vec![5]);
        assert_eq!(b.as_ref_vec(), vec![&7, &5]);
        assert_eq!(c.rest().unwrap().to_vec(), vec![7, 5]);
        assert_eq!(c.to_vec(), vec![9, 7, 5]);
        assert_eq!(d.to_vec(), vec![13, 7, 5]);
        assert_eq!(e.to_vec(), vec![14, 9, 7, 5]);

        assert_eq!(e.contains(&14), true);
        assert_eq!(e.contains(&13), false);
        assert_eq!(e.contains(&5), true);
    }

    #[test]
    fn t_alist() {
        let a = cons((5, "five"), &List::Null);
        let b = cons((2, "two"), &a);
        let c = cons((3, "three"), &b);
        assert_eq!(c.alist_get(&5), Some(&"five"));
        assert_eq!(c.alist_get(&3), Some(&"three"));
        assert_eq!(c.alist_get(&4), None);
    }

    use std::{fmt::Display, sync::Arc};

    use itertools::Itertools;

    enum TreeNode<N: AsRef<str>, T> {
        Leaf(T),
        Split {
            name: N,
            a: Arc<TreeNode<N, T>>,
            b: Arc<TreeNode<N, T>>,
        },
    }

    fn leaf<N: AsRef<str>, T>(val: T) -> Arc<TreeNode<N, T>> {
        TreeNode::Leaf(val).into()
    }

    fn split<N: AsRef<str>, T>(
        name: N,
        a: Arc<TreeNode<N, T>>,
        b: Arc<TreeNode<N, T>>,
    ) -> Arc<TreeNode<N, T>> {
        TreeNode::Split { name, a, b }.into()
    }

    #[derive(Clone)]
    struct FoundNode<'name> {
        direction: &'static str,
        name: &'name str,
    }

    impl<'name> Display for FoundNode<'name> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let Self { direction, name } = self;
            write!(f, "{name} »{direction}» ")
        }
    }

    /// Give a human-readable path to the first leaf for which `pred` returns true.
    fn find_in_tree<N: AsRef<str>, T: ToString>(
        tree: &TreeNode<N, T>,
        pred: impl Fn(&T) -> bool + Copy,
        parents: &List<FoundNode>,
    ) -> Option<String> {
        match tree {
            TreeNode::Leaf(l) => {
                if pred(l) {
                    Some(
                        parents.to_vec().iter().rev().join("") + &l.to_string(),
                    )
                } else {
                    None
                }
            }
            TreeNode::Split { name, a, b } => find_in_tree(
                a,
                pred,
                &cons(
                    FoundNode {
                        direction: "a",
                        name: name.as_ref(),
                    },
                    parents,
                ),
            )
            .or_else(|| {
                find_in_tree(
                    b,
                    pred,
                    &cons(
                        FoundNode {
                            direction: "b",
                            name: name.as_ref(),
                        },
                        parents,
                    ),
                )
            }),
        }
    }

    #[test]
    fn t_find_in_tree() {
        let house = split(
            "house",
            split(
                "kitchen",
                split(
                    "cupboard",
                    split("dishes", leaf("plate"), leaf("jug")),
                    split("food", leaf("beans"), leaf("pasta")),
                ),
                leaf("stove"),
            ),
            split(
                "bathroom",
                split("cupboard", leaf("toothbrush"), leaf("comb")),
                split("toilet", leaf("toilet paper"), leaf("brush")),
            ),
        );
        assert_eq!(
            find_in_tree(&house, |n| *n == "comb", &List::Null).as_deref(),
            Some("house »b» bathroom »a» cupboard »b» comb"),
        );
    }
}
