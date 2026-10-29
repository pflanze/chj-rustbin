//! Make a thread-local variable to allow setting parameters via a guard
//!
//! Unlike a dynamic scope via a callback, this does not enforce strict nesting
//! (the guard can be moved). For that reason, a btree and incremental id is
//! used to store the settings; when a guard is created, a new entry with a
//! newer id is stored, when a guard is droped, the entry is removed. Reads
//! always get the newest existing setting (i.e. with the highest id).  This
//! approach isn't very fast, but seems like it would generally exhibit the most
//! expected behaviour.
//!
//! `$T` must implement Clone.  The `$DEFAULT` expression is run every time
//! `$Name::get()` is run with no active settings.

#[macro_export]
macro_rules! define_scoped_setting {
    { $(#[$attr:meta])*
      $Name:ident => $T:ty, $DEFAULT:expr => $NAME:ident, $NAME_ID:ident } => {

        thread_local! {
            static $NAME: RefCell<std::collections::BTreeMap<u128, $T>> =
                std::collections::BTreeMap::new().into();
            // Running id used in the map
            static $NAME_ID: Cell<u128> = 0.into();
        }

        $(#[$attr])*
        pub struct $Name(u128);

        impl $Name {
            pub fn new(value: $T) -> Self {
                let id = $NAME_ID.get();
                $NAME_ID.set(id + 1);
                $NAME.with_borrow_mut(|b| {
                    b.insert(id, value);
                });
                $Name(id)
            }

            pub fn get() -> $T {
                $NAME.with_borrow(|s| s.last_key_value().map(|(_, val)| val.clone()).unwrap_or_else(|| $DEFAULT))
            }
        }

        impl Drop for $Name {
            fn drop(&mut self) {
                $NAME.with_borrow_mut(|b| {
                    b.remove(&self.0);
                });
            }
        }
    }
}
