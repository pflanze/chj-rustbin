//! Allocate byte slices in shared thread-local region allocators
//!
//! Short-lived tasks, e.g. from Rayon, working on a shared job, may
//! want to efficiently allocate memory for that job, i.e. from a
//! region that lives as long as the job's temporary data or
//! result. Each underlying thread should allocate from the same
//! sub-region for efficiency, while keeping that sub-region around
//! for the next task working on the same job in the same thread.
//!
//! This is what this module tries to achieve: a single
//! `SharedRegions` is instantiated for the whole job. It is
//! shared to each spawned task, which calls `get_region`, which gets
//! back the previously allocated region for the current thread in a
//! reasonably efficient way[1] (and if there is none yet, a new
//! region is allocated transparently). The life time of the allocated
//! memory is the one of the `SharedRegions` instance.
//!
//! [1]: Currently a mutex is locked for a short time to get it, once
//! again to drop it; there's potential to improve that more.

use std::{
    collections::{hash_map::Entry, HashMap},
    ffi::OsStr,
    io::stderr,
    marker::PhantomData,
    mem::swap,
    os::unix::ffi::OsStrExt,
    path::Path,
    sync::Mutex,
    thread::ThreadId,
};

use log::trace;
use memmap2::{MmapMut, MmapOptions};

use crate::{
    parse::parse_error::FileLocation, unsafe_util::raw_slice_mut::RawSliceMut,
    unsync_unsend::UnSyncUnSend,
};

/// A holder for all `SharedRegion`s across the program, to cache them
/// for each thread.
pub struct SharedRegions {
    /// Where this was allocated, for debugging. Use `file_location!`
    /// macro to get it.
    allocation_context: &'static FileLocation,

    /// Whether to try to print memory stats to stderr when dropped
    print_stats_on_drop: bool,

    /// How many bytes to allocate unless the requested size was larger
    min_region_size: usize,

    /// All regions not in current use by threads. The last one is
    /// retrieved again via SharedRegion::new (which must remove the
    /// entry here to avoid risking double use of the mut reference),
    /// and drop puts it back. For ownership, and to iterate over for
    /// stats.
    #[allow(clippy::type_complexity)]
    regions: Mutex<HashMap<ThreadId, (Vec<MmapMut>, Vec<InnerSharedRegion>)>>,
}

impl Drop for SharedRegions {
    fn drop(&mut self) {
        if self.print_stats_on_drop {
            _ = self.eprint_stats();
        }
    }
}

// SAFETY: It's OK to send it or its reference around as `regions` is
// private and InnerSharedRegion is only ever accessed by the same
// thread that created it, which shields it from the outer Send and
// Sync.
unsafe impl Send for SharedRegions {}
unsafe impl Sync for SharedRegions {}

impl SharedRegions {
    pub fn new(
        min_region_size: usize,
        allocation_context: &'static FileLocation,
        print_stats_on_drop: bool,
    ) -> Self {
        Self {
            allocation_context,
            print_stats_on_drop,
            min_region_size,
            regions: Default::default(),
        }
    }

    pub fn get_region(&self) -> SharedRegion<'_> {
        SharedRegion::new(self)
    }

    /// Total of all allocations of all threads, except for the
    /// regions in current use by threads. Careful, blocks
    /// `get_region` and `SharedRegion::drop` while calculating.
    pub fn total_bytes_allocated(&self) -> usize {
        let regions = self.regions.lock().expect("no panics");
        regions
            .values()
            .map(|(_, inners)| -> usize {
                inners.iter().map(|inner| inner.bytes_allocated()).sum()
            })
            .sum()
    }

    /// Print number of bytes allocated, together with the location
    /// where this instance was allocated. See caveat for
    /// `total_bytes_allocated`.
    pub fn eprint_stats(&self) -> Result<(), std::io::Error> {
        let n = self.total_bytes_allocated();
        use std::io::Write;
        writeln!(
            &mut stderr(),
            "{n} bytes allocated in SharedRegions from {}",
            self.allocation_context
        )
    }
}

struct InnerSharedRegion {
    orig_start: *mut u8,
    current: RawSliceMut,
}

impl InnerSharedRegion {
    fn new(num_bytes: usize, min_region_size: usize) -> (MmapMut, Self) {
        let region_size = min_region_size.max(num_bytes);
        let mut _storage =
            // XX OK to panic for out of memory?
            MmapOptions::new().len(region_size).map_anon().expect("succeeds unless out of memory");
        let current: RawSliceMut = (&mut *_storage).into();
        trace!(
            "InnerSharedRegion::new({num_bytes}, {min_region_size}), \
             current={current:?}"
        );
        (
            _storage,
            Self {
                orig_start: current.data(),
                current,
            },
        )
    }

    fn bytes_allocated(&self) -> usize {
        let Self {
            orig_start,
            current,
        } = self;
        unsafe { current.data().offset_from(*orig_start) }
            .try_into()
            .expect("always fits half the address space")
    }
}

/// A fast allocator for byte slices for the current thread (!Sync,
/// !Send).
pub struct SharedRegion<'g> {
    _only_on_same_thread: PhantomData<UnSyncUnSend>,
    shared_regions: &'g SharedRegions,
    inner_shared_region: Option<InnerSharedRegion>,
}

impl<'g> Drop for SharedRegion<'g> {
    fn drop(&mut self) {
        if let Some(inner_shared_region) = self.inner_shared_region.take() {
            let cutoff = 1.max(self.shared_regions.min_region_size / 2);
            let InnerSharedRegion {
                current,
                orig_start: _,
            } = inner_shared_region;
            let do_put_back = current.len() > cutoff;
            trace!(
                "SharedRegion::drop(current = {current:?}): cutoff={cutoff}, \
                 do_put_back={do_put_back}"
            );
            if do_put_back {
                let thread_id = std::thread::current().id();
                let mut regions =
                    self.shared_regions.regions.lock().expect("no panics");
                match regions.entry(thread_id) {
                    Entry::Occupied(occupied_entry) => {
                        let boxes_and_regions = occupied_entry.into_mut();
                        boxes_and_regions.1.push(inner_shared_region);
                    }
                    Entry::Vacant(_) => {
                        unreachable!("entry for thread was made when creating the SharedRegion")
                    }
                }
            }
        }
    }
}

impl<'g> SharedRegion<'g> {
    fn new(shared_regions: &'g SharedRegions) -> Self {
        let inner_shared_region = {
            let thread_id = std::thread::current().id();
            let mut boxes_and_regions =
                shared_regions.regions.lock().expect("no panics");
            let boxes = match boxes_and_regions.entry(thread_id) {
                Entry::Occupied(occupied_entry) => {
                    let (boxes, regions) = occupied_entry.into_mut();
                    if let Some(inner_shared_region) = regions.pop() {
                        return Self {
                            _only_on_same_thread: PhantomData,
                            shared_regions,
                            inner_shared_region: Some(inner_shared_region),
                        };
                    }
                    boxes
                }
                Entry::Vacant(vacant_entry) => {
                    &mut vacant_entry.insert((Vec::new(), Vec::new())).0
                }
            };
            let min_region_size = shared_regions.min_region_size;

            // Pass 0 as minimal size, we have no hint about what's needed
            let (bx, inner_shared_region) =
                InnerSharedRegion::new(0, min_region_size);
            boxes.push(bx);
            inner_shared_region
        };
        Self {
            _only_on_same_thread: PhantomData,
            shared_regions,
            inner_shared_region: Some(inner_shared_region),
        }
    }

    /// `ALIGN` is the value returned by `core::mem::align_of` (how
    /// many bytes the address must be a multiple of; power of 2, at
    /// least 1). Only the beginning of the returned slice is
    /// guaranteed to be aligned, its length is always `num_bytes`.
    pub fn allocate<'s, const ALIGN: usize>(
        &'s mut self,
        num_bytes: usize,
    ) -> &'g mut [u8]
    where
        'g: 's,
    {
        const {
            assert!(ALIGN.count_ones() == 1);
        }
        let inner_shared_region =
            self.inner_shared_region.as_mut().expect("always there");
        if let Some(current_aligned) =
            inner_shared_region.current.aligned::<ALIGN>()
        {
            if num_bytes <= current_aligned.len() {
                let (res, rest) = current_aligned.split_at_mut(num_bytes);
                // dbg!(res);
                inner_shared_region.current = rest;
                return unsafe {
                    // Safe because ownership of the storage is in
                    // SharedRegions, and 'g is tied to that. And we
                    // only hand out this slice to the same memory area a
                    // single time.
                    res.to_slice_mut()
                };
            }
        }

        // Done with the current region. Create a new one, swap
        // and let it move itself back in if appropriate. (A bit
        // less efficient than it could be?)

        // Surplus is the largest possible gap at the beginning so
        // that the alignment is correct
        let align_surplus = ALIGN - 1;
        let num_bytes_with_surplus = num_bytes + align_surplus;

        let min_region_size = self.shared_regions.min_region_size;
        let (bx, inner_shared_region) =
            InnerSharedRegion::new(num_bytes_with_surplus, min_region_size);
        {
            let thread_id = std::thread::current().id();
            let mut boxes_and_regions =
                self.shared_regions.regions.lock().expect("no panics");
            let boxes_and_regions =
                boxes_and_regions.get_mut(&thread_id).expect(
                    "entry was already made when creating this SharedRegion",
                );
            boxes_and_regions.0.push(bx);
        }
        let mut other = Self {
            _only_on_same_thread: PhantomData,
            shared_regions: self.shared_regions,
            inner_shared_region: Some(inner_shared_region),
        };
        swap(self, &mut other);
        self.allocate::<ALIGN>(num_bytes)
    }

    /// Return the num_bytes from the end of the last allocation. Safety:
    /// an allocation on the same `self` must have occurred
    /// previously; `num_bytes` must be <= `num_bytes` of that last
    /// allocation; and the last `num_bytes` of that last allocation
    /// must be unused.
    pub unsafe fn return_unused(&mut self, num_bytes: usize) {
        let inner_shared_region =
            self.inner_shared_region.as_mut().expect("always there");
        inner_shared_region.current =
            inner_shared_region.current.start_sub(num_bytes);
    }

    pub fn allocate_path<'s, P: AsRef<Path>>(&'s mut self, path: P) -> &'g Path
    where
        'g: 's,
    {
        let path = path.as_ref();
        // XX as_bytes is Unix-only, right? Not sure
        let bytes = path.as_os_str().as_bytes();
        let len = bytes.len();
        let sl = self.allocate::<1>(len);
        sl.copy_from_slice(bytes);
        let os_str = OsStr::from_bytes(sl);
        os_str.as_ref()
    }
}
