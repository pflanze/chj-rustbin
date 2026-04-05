//! Allocate byte slices in thread-local region allocators

use std::{
    collections::HashMap, ffi::OsStr, marker::PhantomData, mem::swap,
    os::unix::ffi::OsStrExt, path::Path, slice::from_raw_parts_mut,
    sync::Mutex, thread::ThreadId,
};

use memmap2::{MmapMut, MmapOptions};

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
    unsafe fn to_slice_mut<'t>(self) -> &'t mut [u8] {
        let Self { data, len } = self;
        from_raw_parts_mut(data, len)
    }

    fn split_at_mut(self, pos: usize) -> (RawSliceMut, RawSliceMut) {
        let Self { data, len } = self;
        assert!(pos <= len);
        // unnecessary check?, as data+len must be within usize and
        // thus data+pos, too.
        assert!(pos <= isize::MAX as usize);
        let head = RawSliceMut { data, len: pos };
        let rest = RawSliceMut {
            data: unsafe {
                // Assuming that `self` is valid, this is safe because
                // pos is within len as checked above, and it is
                // within isize as checked above or as implied by
                // `self` being valid.
                data.add(pos)
            },
            len: len - pos,
        };
        (head, rest)
    }
}

/// A holder for all `LeakedRegion`s across the program, to cache them
/// for each thread.
pub struct GlobalLeakedRegions {
    /// How many bytes to allocate unless the requested size was larger
    min_region_size: usize,

    // (XX update docs) Store the regions here so that the last one
    // can be retrieved again via LeakedRegion::new (which must remove
    // the entry here to avoid risking double use of the mut
    // reference), but also to perhaps iterate through all of it in
    // the future. Only store leaked memory here, though, since
    // threads exiting must not deallocate the memory!--XX ah, now
    // stores back the mostly-used-up-slice, not the whole region,
    // back here, so won't be useful for reading!
    regions: Mutex<HashMap<ThreadId, (Vec<MmapMut>, Vec<InnerLeakedRegion>)>>,
}

// SAFETY: It's OK to send it or its reference around as `regions` is
// private and InnerLeakedRegion is only ever accessed by the same
// thread that created it, which shields it from the outer Send and
// Sync.
unsafe impl Send for GlobalLeakedRegions {}
unsafe impl Sync for GlobalLeakedRegions {}

impl GlobalLeakedRegions {
    pub fn new(min_region_size: usize) -> Self {
        Self {
            min_region_size,
            regions: Default::default(),
        }
    }

    pub fn get_region(&self) -> LeakedRegion<'_> {
        LeakedRegion::new(self)
    }
}

struct InnerLeakedRegion {
    current: RawSliceMut,
}

impl InnerLeakedRegion {
    fn new(num_bytes: usize, min_region_size: usize) -> (MmapMut, Self) {
        let region_size = min_region_size.max(num_bytes);
        let mut _storage =
            // XX OK to panic for out of memory?
            MmapOptions::new().len(region_size).map_anon().expect("succeeds unless out of memory");
        let current = (&mut *_storage).into();
        (_storage, Self { current })
    }
}

/// A fast allocator for byte slices for the current thread (!Sync,
/// !Send).
pub struct LeakedRegion<'g> {
    _only_on_same_thread: PhantomData<UnSyncUnSend>,
    global_leaked_regions: &'g GlobalLeakedRegions,
    inner_leaked_region: Option<InnerLeakedRegion>,
}

impl<'g> Drop for LeakedRegion<'g> {
    fn drop(&mut self) {
        if let Some(inner_leaked_region) = self.inner_leaked_region.take() {
            let cutoff = 1.max(self.global_leaked_regions.min_region_size / 2);
            if inner_leaked_region.current.len > cutoff {
                let thread_id = std::thread::current().id();
                let mut regions = self
                    .global_leaked_regions
                    .regions
                    .lock()
                    .expect("no panics");
                match regions.entry(thread_id) {
                    std::collections::hash_map::Entry::Occupied(
                        occupied_entry,
                    ) => {
                        let boxes_and_regions = occupied_entry.into_mut();
                        boxes_and_regions.1.push(inner_leaked_region);
                    }
                    std::collections::hash_map::Entry::Vacant(_) => {
                        unreachable!("entry for thread was made when creating the LeakedRegion")
                    }
                }
            }
        }
    }
}

impl<'g> LeakedRegion<'g> {
    fn new(global_leaked_regions: &'g GlobalLeakedRegions) -> Self {
        let inner_leaked_region = {
            let thread_id = std::thread::current().id();
            let mut boxes_and_regions =
                global_leaked_regions.regions.lock().expect("no panics");
            let boxes = match boxes_and_regions.entry(thread_id) {
                std::collections::hash_map::Entry::Occupied(occupied_entry) => {
                    let (boxes, regions) = occupied_entry.into_mut();
                    if let Some(inner_leaked_region) = regions.pop() {
                        return Self {
                            _only_on_same_thread: PhantomData,
                            global_leaked_regions,
                            inner_leaked_region: Some(inner_leaked_region),
                        };
                    }
                    boxes
                }
                std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                    &mut vacant_entry.insert((Vec::new(), Vec::new())).0
                }
            };
            let min_region_size = global_leaked_regions.min_region_size;

            // Pass 0 as minimal size, we have no hint about what's needed
            let (bx, inner_leaked_region) =
                InnerLeakedRegion::new(0, min_region_size);
            boxes.push(bx);
            inner_leaked_region
        };
        Self {
            _only_on_same_thread: PhantomData,
            global_leaked_regions,
            inner_leaked_region: Some(inner_leaked_region),
        }
    }

    pub fn allocate<'s>(&'s mut self, num_bytes: usize) -> &'g mut [u8]
    where
        'g: 's,
    {
        let inner_leaked_region =
            self.inner_leaked_region.as_mut().expect("always there");
        if num_bytes <= inner_leaked_region.current.len {
            let (res, rest) =
                inner_leaked_region.current.split_at_mut(num_bytes);
            // dbg!(res);
            inner_leaked_region.current = rest;
            unsafe {
                // Safe because ownership of the storage is in
                // GlobalLeakedRegions, and 'g is tied to that. And we
                // only hand out this slice to the same memory area a
                // single time.
                res.to_slice_mut()
            }
        } else {
            // Done with the current region. Create a new one, swap
            // and let it move itself back in if appropriate. (A bit
            // less efficient than it could be?)
            let min_region_size = self.global_leaked_regions.min_region_size;
            let (bx, inner_leaked_region) =
                InnerLeakedRegion::new(num_bytes, min_region_size);
            {
                let thread_id = std::thread::current().id();
                let mut boxes_and_regions = self
                    .global_leaked_regions
                    .regions
                    .lock()
                    .expect("no panics");
                let boxes_and_regions = boxes_and_regions
                    .get_mut(&thread_id)
                    .expect(
                    "entry was already made when creating this LeakedRegion",
                );
                boxes_and_regions.0.push(bx);
            }
            let mut other = Self {
                _only_on_same_thread: PhantomData,
                global_leaked_regions: self.global_leaked_regions,
                inner_leaked_region: Some(inner_leaked_region),
            };
            swap(self, &mut other);
            self.allocate(num_bytes)
        }
    }

    pub fn allocate_path<'s, P: AsRef<Path>>(&'s mut self, path: P) -> &'g Path
    where
        'g: 's,
    {
        let path = path.as_ref();
        // XX as_bytes is Unix-only, right? Not sure
        let bytes = path.as_os_str().as_bytes();
        let len = bytes.len();
        let sl = self.allocate(len);
        sl.copy_from_slice(bytes);
        let os_str = OsStr::from_bytes(sl);
        os_str.as_ref()
    }
}
