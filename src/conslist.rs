/// Cons list. Only works with normal references. That's because
/// making it generic for the reference/container type appears
/// unworkable, even though feasible in principle, due to requiring
/// explicit type parameters on every single cons call. The solution
/// will be to generate the code for versions using Rc, Arc or
/// whatever when needed. (Or, use dyn?, or perhaps/probably rather,
/// enum.)

pub enum List<'t, T> {
    Pair(T, &'t List<'t, T>),
    Null
}

pub fn cons<'l, T>(v: T, r: &'l List<T>) -> List<'l, T> {
    List::Pair(v, r)
}

impl<'t, T> List<'t, T> {
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
    pub fn rest(&self) -> Option<&List<T>> {
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
        loop {
            match r {
                List::Pair(v, r2) => {
                    vs.push(v);
                    r = r2;
                }
                List::Null => break,
            }
        }
        vs
    }
    pub fn to_vec(&self) -> Vec<T>
    where T: Clone
    {
        let mut vs: Vec<T> = Vec::new();
        let mut r = self;
        loop {
            match r {
                List::Pair(v, r2) => {
                    vs.push(v.clone());
                    r = r2;
                }
                List::Null => break,
            }
        }
        vs
    }
}

impl<'t, K: PartialEq, V> List<'t, (K, V)> {
    /// In a List of (K, V) pairs, get the first V for which the K ==
    /// key.
    pub fn alist_get(&self, key: &K) -> Option<&V> {
        match self {
            List::Pair((k, v), rest) => if k == key {
                Some(v)
            } else {
                rest.alist_get(key)
            },
            List::Null => None,
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
}
