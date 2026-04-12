//! Searchable container

// Operations are implemented separately (see get_release.rs)

#[derive(Debug)]
pub struct Infos<T> {
    pub infos: Vec<T>,
}
