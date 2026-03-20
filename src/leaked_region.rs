//! Allocate and leak byte slices in thread-local allocators with static lifetime
//!
//! Todo: this needs proper testing.

use std::{
    collections::HashMap, ffi::OsStr, marker::PhantomData, mem::swap,
    os::unix::ffi::OsStrExt, path::Path, slice::from_raw_parts_mut,
    sync::Mutex, thread::ThreadId,
};

use crate::unsync_unsend::UnSyncUnSend;

#[derive(Debug, Clone, Copy)]
struct RawSliceMut {
    data: *mut u8,
    len: usize,
}

impl From<&mut [u8]> for RawSliceMut {
    fn from(sl: &mut [u8]) -> Self {
        let data = sl.as_mut_ptr();
        let len = sl.len();
        Self { data, len }
    }
}

impl RawSliceMut {
    unsafe fn to_slice_mut(self) -> &'static mut [u8] {
        let Self { data, len } = self;
        from_raw_parts_mut(data, len)
    }

    fn split_at_mut(self, pos: usize) -> (RawSliceMut, RawSliceMut) {
        let Self { data, len } = self;
        assert!(pos <= len);
        let head = RawSliceMut { data, len: pos };
        let rest = RawSliceMut {
            data: unsafe { data.add(pos) },
            len: len - pos,
        };
        (head, rest)
    }
}

/// A holder for all `LeakedRegion`s across the program, to cache them
/// for each thread.
pub struct GlobalLeakedRegions {
    // (XX update docs) Store the regions here so that the last one
    // can be retrieved again via LeakedRegion::new (which must remove
    // the entry here to avoid risking double use of the mut
    // reference), but also to perhaps iterate through all of it in
    // the future. Only store leaked memory here, though, since
    // threads exiting must not deallocate the memory!--XX ah, now
    // stores back the mostly-used-up-slice, not the whole region,
    // back here, so won't be useful for reading!
    regions: Mutex<HashMap<ThreadId, Vec<InnerLeakedRegion>>>,
}

// SAFETY: It's OK to send it or its reference around as `regions` is
// private and InnerLeakedRegion is only ever accessed by the same
// thread that created it, which shields it from the outer Send and
// Sync.
unsafe impl Send for GlobalLeakedRegions {}
unsafe impl Sync for GlobalLeakedRegions {}

impl GlobalLeakedRegions {
    pub fn new() -> Self {
        Self {
            regions: Default::default(),
        }
    }

    pub fn get_region(&self) -> LeakedRegion<'_> {
        LeakedRegion::new(self)
    }
}

// Do not make public? Necessary so that we can have Clone with no
// problem?
#[derive(Clone)]
struct InnerLeakedRegion {
    /// Must keep the original `&mut [u8]` around to avoid UB? But
    /// don't use it, use the RawSliceMut which is the remaining bit,
    /// OK? -- Is non-mut enough?
    _original_slice: &'static [u8],
    current_region: RawSliceMut,
}

impl InnerLeakedRegion {
    // How many bytes to allocate unless the requested size was larger
    const MIN_REGION_SIZE: usize = 1048576;

    fn new(num_bytes: usize) -> Self {
        let region_size = Self::MIN_REGION_SIZE.max(num_bytes);
        let new_region: Vec<u8> = vec![0; region_size];
        let _original_slice = Box::leak(new_region.into_boxed_slice());
        let current_region = _original_slice.into();
        Self {
            _original_slice,
            current_region,
        }
    }
}

/// A fast allocator for 'static byte slices for the current thread
/// (!Sync, !Send).
pub struct LeakedRegion<'g> {
    _only_on_same_thread: PhantomData<UnSyncUnSend>,
    global_leaked_regions: &'g GlobalLeakedRegions,
    inner_leaked_region: InnerLeakedRegion,
}

impl<'g> Drop for LeakedRegion<'g> {
    fn drop(&mut self) {
        // XX configurable?
        if self.inner_leaked_region.current_region.len > 10000 {
            let thread_id = std::thread::current().id();
            let this = self.inner_leaked_region.clone();
            let mut regions = self
                .global_leaked_regions
                .regions
                .lock()
                .expect("no panics");
            match regions.entry(thread_id) {
                std::collections::hash_map::Entry::Occupied(occupied_entry) => {
                    occupied_entry.into_mut().push(this);
                }
                std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(vec![this]);
                }
            }
        }
    }
}

impl<'g> LeakedRegion<'g> {
    fn new(global_leaked_regions: &'g GlobalLeakedRegions) -> Self {
        {
            let thread_id = std::thread::current().id();
            let mut regions =
                global_leaked_regions.regions.lock().expect("no panics");
            if let Some(r) = regions.get_mut(&thread_id) {
                if let Some(inner_leaked_region) = r.pop() {
                    return Self {
                        _only_on_same_thread: PhantomData,
                        global_leaked_regions,
                        inner_leaked_region,
                    };
                }
            }
        }
        // Pass 0 as minimal size, we have no hint about what's needed
        let inner_leaked_region = InnerLeakedRegion::new(0);
        Self {
            _only_on_same_thread: PhantomData,
            global_leaked_regions,
            inner_leaked_region,
        }
    }

    pub fn allocate(&mut self, num_bytes: usize) -> &'static mut [u8] {
        if num_bytes <= self.inner_leaked_region.current_region.len {
            let (res, rest) = self
                .inner_leaked_region
                .current_region
                .split_at_mut(num_bytes);
            // dbg!(res);
            self.inner_leaked_region.current_region = rest;
            unsafe { res.to_slice_mut() }
        } else {
            // Done with the current region. Create a new one, swap
            // and let it move itself back in if appropriate. (A bit
            // less efficient than it could be?)
            let mut other = Self {
                _only_on_same_thread: PhantomData,
                global_leaked_regions: self.global_leaked_regions,
                inner_leaked_region: InnerLeakedRegion::new(num_bytes),
            };
            swap(self, &mut other);
            self.allocate(num_bytes)
        }
    }

    pub fn allocate_path(&mut self, path: &Path) -> &'static Path {
        // XX as_bytes is Unix-only, right? Not sure
        let bytes = path.as_os_str().as_bytes();
        let len = bytes.len();
        let sl = self.allocate(len);
        sl.copy_from_slice(bytes);
        let os_str = OsStr::from_bytes(sl);
        os_str.as_ref()
    }
}
