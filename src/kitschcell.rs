//! Lazy once more. bc lifetimes.

use std::mem::swap;

pub enum KitschCell<T, F: FnOnce() -> T> {
    Unevaluated(F),
    Evaluating,
    Evaluated(T),
}

pub trait KitschCache<T> {
    fn get(&mut self) -> &mut T;
}

impl<T, F: FnOnce() -> T> KitschCache<T> for KitschCell<T, F> {
    fn get(&mut self) -> &mut T {
        match self {
            KitschCell::Unevaluated(_) => {
                let mut replacement = KitschCell::Evaluating;
                swap(self, &mut replacement);
                let f = match replacement {
                    KitschCell::Unevaluated(f) => f,
                    _ => unreachable!(),
                };
                let val = f();
                *self = KitschCell::Evaluated(val);
                match self {
                    KitschCell::Evaluated(val) => val,
                    _ => unreachable!(),
                }
            }
            KitschCell::Evaluated(val) => val,
            KitschCell::Evaluating => panic!(
                "already evaluating, there is a loop calling `get` \
                 or a previous call panicked"
            ),
        }
    }
}

pub struct KitschValue<T>(pub T);

impl<T> KitschCache<T> for KitschValue<T> {
    #[inline]
    fn get(&mut self) -> &mut T {
        &mut self.0
    }
}
