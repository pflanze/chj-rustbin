use std::{hash::Hash, collections::{HashMap, hash_map::{Entry, OccupiedEntry}, BTreeMap, btree_map}, borrow::Borrow, time::Duration, fmt::Display, convert::TryInto, ops::Add};


// A HashMap::get_mut variant that allows to work around the issue
// that rustc (pre polonius) does not let go of the reference in the
// None case; so we use an Err instead and pick up the reference from
// there.
pub fn hashmap_get_mut<'m, K: Eq + Hash, P: Eq + Hash + ?Sized, V>(
    m: &'m mut HashMap<K, V>,
    k: &P,
) -> Result<&'m mut V,
            &'m mut HashMap<K, V>>
where K: Borrow<P>
{
    let pm: *mut _ = m;
    // Safe because in the true branch we track using the lifetimes
    // (just like in the original get_mut), and in the false branch we
    // just pass the input value.
    if let Some(v) = unsafe{&mut *pm}.get_mut(k) {
        Ok(v)
    } else {
        Err(unsafe{&mut *pm})
    }
}

// Same (see hashmap_get_mut) for BTreeMap.
pub fn btreemap_get_mut<'m, K: Ord, P: Ord + ?Sized, V>(
    m: &'m mut BTreeMap<K, V>,
    k: &P,
) -> Result<&'m mut V,
            &'m mut BTreeMap<K, V>>
where K: Borrow<P>
{
    let pm: *mut _ = m;
    // Safe because in the true branch we track using the lifetimes
    // (just like in the original get_mut), and in the false branch we
    // just pass the input value.
    if let Some(v) = unsafe{&mut *pm}.get_mut(k) {
        Ok(v)
    } else {
        Err(unsafe{&mut *pm})
    }
}


// Modified copy of #[unstable(feature = "map_try_insert", issue =
// "82766")] from
// https://doc.rust-lang.org/src/std/collections/hash/map.rs.html#1132-1137,
// avoiding OccupiedError because that's also unstable. FUTURE:
// replace with try_insert.
pub fn hashmap_try_insert<'m, K: Eq + Hash, V>(
    m: &'m mut HashMap<K, V>,
    key: K,
    value: V
) -> Result<&mut V, OccupiedEntry<K, V>>
{
    match m.entry(key) {
        Entry::Occupied(entry) => Err(entry),
        Entry::Vacant(entry) => Ok(entry.insert(value)),
    }
}


// Modified copy of #[unstable(feature = "map_try_insert", issue =
// "82766")] from
// https://doc.rust-lang.org/src/alloc/collections/btree/map.rs.html#1016-1018;
// see comments on hashmap_try_insert.
pub fn btreemap_try_insert<'m, K: Ord, V>(
    m: &'m mut BTreeMap<K, V>,
    key: K,
    value: V
) -> Result<&'m mut V, btree_map::OccupiedEntry<K, V>>
{
    match m.entry(key) {
        btree_map::Entry::Occupied(entry) => Err(entry),
        btree_map::Entry::Vacant(entry) => Ok(entry.insert(value)),
    }
}


pub fn hashmap_get_mut_vivify<'m, K: Eq + Hash + Clone, V>(
    m: &'m mut HashMap<K, V>,
    k: &K,
    create: impl FnOnce() -> V
) -> &'m mut V
{
    match hashmap_get_mut(m, k) {
        Ok(val) => val,
        Err(m) => {
            m.insert(k.clone(), create());
            m.get_mut(k).expect("just inserted")
        }
    }
}


/// Insert a key-value mapping, or if already existing, add the value
/// to the existing value via Add trait.
pub fn hashmap_add<K: Hash + Eq, V: Add<Output = V> + Copy>(
    m: &mut HashMap<K, V>, k: K, v: V
) {
    match hashmap_get_mut(m, &k) {
        Ok(oldv) => {
            // *oldv += v;  What's needed for this?
            *oldv = oldv.add(v);
        }
        Err(m) => {
            m.insert(k, v);
        }
    }
}



// Sigh, for exponential backoff, everybody doing this for themselves?
pub fn duration_mul_div(orig: Duration, multiplier: u64, divider: u64)
                        -> Option<Duration>
{
    let nanos: u64 = orig.as_nanos().checked_mul(multiplier as u128)?
        .checked_div(divider as u128)?
        .try_into().ok()?;
    Some(Duration::from_nanos(nanos))
}


pub fn first<T>(items: &[T]) -> &T {
    &items[0]
}

pub fn rest<T>(items: &[T]) -> &[T] {
    &items[1..]
}



pub fn debug_stringlikes<S: Display>(v: &[S]) -> Vec<String> {
    v.iter().map(|s| s.to_string()).collect()
}


/// A loop that caches errors and retries with exponential
/// backoff. (Backoff parameters and error messaging hard coded for
/// now, as is anyhow::Result.)
#[macro_export]
macro_rules! loop_try {
    ( $($body_parts:tt)* ) => {{
        let default_error_sleep_duration = Duration::from_millis(500);
        let mut error_sleep_duration = default_error_sleep_duration;
        loop {
            match (|| -> Result<()> { $($body_parts)* })() {
                Ok(()) => {
                    error_sleep_duration = default_error_sleep_duration;
                }
                Err(e) => {
                    eprintln!("loop_try: got error {e:#}, sleeping for \
                               {error_sleep_duration:?}");
                    thread::sleep(error_sleep_duration);
                    error_sleep_duration =
                        crate::util::duration_mul_div(error_sleep_duration,
                                         1200,
                                         1000)
                        .unwrap_or(default_error_sleep_duration);
                }
            }
        }
    }}
}


// For cases where the context is not Option, hence `?` does not
// work. -- vs. try_option ?!
#[macro_export]
macro_rules! tryoption {
    ($e:expr) => {{
        let res = $e;
        if let Some(val) = res {
            val
        } else {
            return Ok(None)
        }
    }}
}

#[macro_export]
macro_rules! try_do {
    ( $($b:tt)* ) => ( (|| { $($b)* })() )
}

#[macro_export]
macro_rules! try_result {
    ( $($b:tt)* ) => ( (|| -> Result<_> { $($b)* })() )
}

#[macro_export]
macro_rules! try_option {
    ( $($b:tt)* ) => ( (|| -> Option<_> { $($b)* })() )
}

/// Alternative for `?` in genawaiter closures.
#[macro_export]
macro_rules! gen_try_result {
    ( $e:expr, $co:expr ) => {
        match $e {
            Ok(v) => v,
            Err(e) => {
                $co.yield_(Err(e)).await;
                return;
            }
        }
    }
}
