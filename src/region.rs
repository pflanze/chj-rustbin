//! A region with lifetime tracking for ids and sync support.

use std::convert::TryInto;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::sync::{Mutex, MutexGuard};


#[derive(Debug)]
pub struct Region<'region, T> {
    region_phantom: PhantomData<&'region T>,
    region: Mutex<Vec<T>>
}

// Do not implement Hash or Ordering, it would be confusing (this is
// *not* interning, hence there is no way to get to the same id from
// the same source object, and "pointer" comparisons normally do not
// make sense)!
#[derive(Debug, PartialEq)]
pub struct RegionId<'region, T> {
    region_phantom: PhantomData<&'region T>,
    id: u32
}

impl<'region, T> Clone for RegionId<'region, T> {
    fn clone(&self) -> Self {
        Self { region_phantom: Default::default(), id: self.id }
    }
}

impl<'region, T> Copy for RegionId<'region, T> {}


// Have to do my own MappedMutexGuard since that is nightly only.
pub struct MutexRef<'m, T: 'm, R, M: for<'g> Fn(&'g T) -> &'g R>{
    guard: MutexGuard<'m, T>,
    mapper: M
}

impl<'m, T: 'm, R, M: for<'g> Fn(&'g T) -> &'g R> MutexRef<'m, T, R, M> {
    pub fn map(guard: MutexGuard<'m, T>, mapper: M) -> Self {
        MutexRef { guard, mapper }
    }
}

impl<'m, T: 'm, R, M: for<'g> Fn(&'g T) -> &'g R> Deref for MutexRef<'m, T, R, M> {
    type Target = R;

    fn deref(&self) -> &Self::Target {
        (self.mapper)(&self.guard)
    }
}

pub struct MutexRefMut<'m, T: 'm, R, M: for<'g> Fn(&'g mut T) -> &'g mut R>{
    guard: MutexGuard<'m, T>,
    mapper: M
}

impl<'m, T: 'm, R, M: for<'g> Fn(&'g mut T) -> &'g mut R> MutexRefMut<'m, T, R, M> {
    pub fn map(guard: MutexGuard<'m, T>, mapper: M) -> Self {
        MutexRefMut { guard, mapper }
    }
}

// XX ah ok,  is &mut variant enough?  returning as &  anyway. 'weakening' the closure but fine?
// -> no.  worse, deref not implementable huh

impl<'m, T: 'm, R, M: for<'g> Fn(&'g mut T) -> &'g mut R> Deref for MutexRefMut<'m, T, R, M> {
    type Target = R;

    fn deref(&self) -> &Self::Target {
        // (self.mapper_mut)(&self.guard)
        panic!("can't actually make deref workable for MutexRefMut?")
    }
}

impl<'m, T: 'm, R, M: for<'g> Fn(&'g mut T) -> &'g mut R> DerefMut for MutexRefMut<'m, T, R, M> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        (self.mapper)(&mut self.guard)
    }
}


impl<'region, T> Region<'region, T> {
    pub fn new() -> Self {
        Self {
            region_phantom: Default::default(),
            region: Mutex::new(Vec::new())
        }
    }

    /// Panics when allocating more than 2^32 items
    pub fn store(&self, value: T) -> RegionId<'region, T> {
        let mut region = self.region.lock().unwrap();
        let id = RegionId {
            region_phantom: Default::default(),
            id: region.len().try_into().expect("fewer than 2^32 items")
        };
        region.push(value);
        id
    }

    /// Takes a mutex lock for the life time of MutexRef. To avoid
    /// this cost for every access, use `lock` instead.  May panic on
    /// invalid ids.
    pub fn get<'m>(
        &'m self, id: RegionId<'m, T>
    ) -> MutexRef<'m, Vec<T>, T,
                  impl for<'g> Fn(&'g Vec<T>) -> &'g T + 'm> {
        MutexRef::map(self.region.lock().unwrap(), move |r| &r[id.id as usize])
    }

    /// Takes a mutex lock for the life time of MutexRefMut. To avoid
    /// this cost for every access, use `lock` instead.  May panic on
    /// invalid ids.
    pub fn get_mut<'m>(
        &'m self, id: RegionId<'m, T>
    ) -> MutexRefMut<'m, Vec<T>, T,
                  impl for<'g> Fn(&'g mut Vec<T>) -> &'g mut T + 'm> {
        MutexRefMut::map(self.region.lock().unwrap(), move |r| &mut r[id.id as usize])
    }

    /// Lock access to the region for the life time of the
    /// `RegionGuard`. This makes multiple accesses of existing
    /// entries more efficient.
    pub fn lock<'m>(&'m self) -> RegionGuard<'m, T> {
        RegionGuard {
            region_guard: self.region.lock().unwrap()
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    // #[test]
    // fn t_wrong_association() {
    //     let region1 = Region::new();
    //     let _p = {
    //         let region2 = Region::new();
    //         let p1 = region1.store(PathBuf::from("hi"));
    //         let p2 = region2.store(PathBuf::from("hi2"));
    //         println!("p1 from region1 = {:?}", &*region1.get(p1));
    //         println!("p1 from region2 = {:?}", &*region2.get(p1));
    //         println!("p2 from region1 = {:?}", &*region1.get(p2));
    //         p1
    //     };
    //     panic!()
    // }

    struct Pair<'region, T> {
        value: T,
        prev: Option<RegionId<'region, Pair<'region, T>>>,
        next: Option<RegionId<'region, Pair<'region, T>>>,
    }

    // impl<'region, T> Pair<'region, T> {
    //     fn cons_left(
    // }
    // no, don't have a Pair just an id
    
    // trait RegionTrait<'region, T> {
    //     fn new() -> Self;
    //     fn store(&self, value: T) -> RegionId<'region, T>;
    //     fn get<'m>(
    //         &'m self, id: RegionId<'m, T>
    //     ) -> MutexRef<'m, Vec<T>, T,
    //                   impl for<'g> Fn(&'g MutexGuard<Vec<T>>) -> &'g T + 'm>;
    // }
    //  ^ can't use impl in traits
    // 
    // trait LinkedList<'region, T>: RegionTrait<'region, T> {
    //     fn cons_left(
    //         &self, val: T, right: Option<RegionId<'region, Pair<'region, T>>>
    //     ) -> RegionId<'region, Pair<'region, T>> {
    //         ...
    //     }
    // }

    fn cons_left<'region, T>(
        region: &Region<'region, Pair<'region, T>>,
        value: T,
        next: Option<RegionId<'region, Pair<'region, T>>>
    ) -> RegionId<'region, Pair<'region, T>> {
        let id = region.store(Pair {
            value,
            next,
            prev: None
        });
        if let Some(next) = next {
            let mut n = region.get_mut(next);
            n.prev = Some(id);
        }
        id
    }
    
    
    #[test]
    fn t_() {
        let region = Region::new();
        let c5 = cons_left(&region, 5, None);
        let c4 = cons_left(&region, 4, Some(c5));
        let c3 = cons_left(&region, 3, Some(c4));
        assert_eq!(region.get(c3).value, 3);
        // These deadlock!
        // assert_eq!(region.get(region.get(c3).next.unwrap()).value, 4);
        // assert_eq!(region.get(region.get(c5).prev.unwrap()).value, 4);

        let c3_next = region.get(c3).next.unwrap();
        assert_eq!(region.get(c3_next).value, 4);

        let c5_prev = region.get(c5).prev.unwrap();
        assert_eq!(region.get(c5_prev).value, 4);

        assert_eq!(region.get({ let x = region.get(c3).next.unwrap(); x } ).value, 4);
    }
}


// -----------------------------------------------------------------------------
// Course-grained locking for better performance for batch accesses

pub struct RegionGuard<'m, T> {
    region_guard: MutexGuard<'m, Vec<T>>,
}

impl<'m, T> RegionGuard<'m, T> {
    /// May panic on invalid ids.
    pub fn get(
        &'m self, id: RegionId<'m, T>
    ) -> &'m T {
        &(*self.region_guard)[id.id as usize]
    }

    /// May panic on invalid ids.
    pub fn get_mut(
        &'m mut self, id: RegionId<'m, T>
    ) -> &'m mut T {
        &mut (*self.region_guard)[id.id as usize]
    }
}

