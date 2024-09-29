
/// A temporary capability to look up
#[derive(Clone, Copy)]
pub struct AList<'t, K, V>(pub &'t [(K, V)]);

impl<'t, K: PartialEq, V> AList<'t, K, V>{
    pub fn get(&self, key: &K) -> Option<&V> {
        self.0.iter().find(|(k, _)| k == key).map(|(_, v)| v)
    }
}

