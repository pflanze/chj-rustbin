
// really not exist?

use std::{collections::{HashMap, BTreeMap}, hash::Hash};

pub trait MapTrait<K, V> {
    fn new() -> Self;
    fn clear(&mut self);
    fn insert(&mut self, k: K, v: V) -> Option<V>;
    fn get(&self, k: &K) -> Option<&V>;
    fn get_mut(&mut self, k: &K) -> Option<&mut V>;
}

impl<K: Eq + Hash, V: Eq> MapTrait<K, V> for HashMap<K, V> {
    fn new() -> Self {
        Self::new()
    }

    fn clear(&mut self) {
        Self::clear(self);
    }

    fn insert(&mut self, k: K, v: V) -> Option<V> {
        Self::insert(self, k, v)
    }

    fn get(&self, k: &K) -> Option<&V> {
        Self::get(self, k)
    }

    fn get_mut(&mut self, k: &K) -> Option<&mut V> {
        Self::get_mut(self, k)
    }
}

impl<K: Eq + Ord, V: Eq> MapTrait<K, V> for BTreeMap<K, V> {
    fn new() -> Self {
        Self::new()
    }

    fn clear(&mut self) {
        Self::clear(self);
    }

    fn insert(&mut self, k: K, v: V) -> Option<V> {
        Self::insert(self, k, v)
    }

    fn get(&self, k: &K) -> Option<&V> {
        Self::get(self, k)
    }

    fn get_mut(&mut self, k: &K) -> Option<&mut V> {
        Self::get_mut(self, k)
    }
}

