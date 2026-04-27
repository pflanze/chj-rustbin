//! Lazy once more. bc lifetimes.

pub enum KitschCell<T, F: FnMut() -> T> {
    Uninitialized(F),
    Initialized(T),
}

pub trait KitschCache<T> {
    fn get(&mut self) -> &mut T;
}

impl<T, F: FnMut() -> T> KitschCache<T> for KitschCell<T, F> {
    fn get(&mut self) -> &mut T {
        match self {
            KitschCell::Uninitialized(f) => {
                let val = f();
                *self = KitschCell::Initialized(val);
                match self {
                    KitschCell::Uninitialized(_) => unreachable!(),
                    KitschCell::Initialized(val) => val,
                }
            }
            KitschCell::Initialized(val) => val,
        }
    }
}
