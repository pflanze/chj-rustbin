//! A trait to represent maps (like HashMap or BTreeMap).

use std::{collections::{HashMap, BTreeMap}, hash::Hash};

pub trait MapTrait<K, V> {
    fn new() -> Self;
    fn clear(&mut self);
    fn insert(&mut self, k: K, v: V) -> Option<V>;
    fn get(&self, k: &K) -> Option<&V>;
    fn get_mut(&mut self, k: &K) -> Option<&mut V>;
}

/// Macro that defines code to delegate trait methods to the
/// same-named and -typed original methods. This requires the map type
/// name to be given twice, and to use K and V as type names, or the
/// macro wouldn't work (macros by example do not appear to have the
/// features necessay to do this properly).
#[macro_export]
macro_rules! implement_map_trait_for {
    { $MapTypeName:ident, impl $($impl_line_tokens:tt)* } =>
    {
        impl $($impl_line_tokens)* {
            fn new() -> Self {
                $MapTypeName::new()
            }

            fn clear(&mut self) {
                $MapTypeName::clear(self);
            }

            fn insert(&mut self, k: K, v: V) -> Option<V> {
                $MapTypeName::insert(self, k, v)
            }

            fn get(&self, k: &K) -> Option<&V> {
                $MapTypeName::get(self, k)
            }

            fn get_mut(&mut self, k: &K) -> Option<&mut V> {
                $MapTypeName::get_mut(self, k)
            }
        }        
    }
}

implement_map_trait_for!{HashMap, impl<K: Eq + Hash, V: Eq> MapTrait<K, V> for HashMap<K, V>}

implement_map_trait_for!{BTreeMap, impl<K: Eq + Ord, V: Eq> MapTrait<K, V> for BTreeMap<K, V>}

