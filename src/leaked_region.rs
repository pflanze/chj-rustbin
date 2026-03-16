//! Allocate and leak byte slices in thread-local allocators with static lifetime
//!
//! Todo: this needs proper testing. Also should move to a global
//! container from which the thread-local regions are derived, for
//! non-static lifetimes and ability to free.

use std::{
    cell::UnsafeCell, ffi::OsStr, marker::PhantomData, os::unix::ffi::OsStrExt,
    path::Path,
};

use crate::{hack_static::hack_static, unsync_unsend::UnSyncUnSend};

// Store the regions here so that the last one can be retrieved again
// via LeakedRegion::new (which must remove the entry here to avoid
// risking double use of the mut reference), but also to perhaps
// iterate through all of it in the future. Only store leaked memory
// here, though, since threads exiting must not deallocate the
// memory!--XX ah, now stores back the mostly-used-up-slice, not the
// whole region, back here, so won't be useful for reading!
thread_local! {
    static REGIONS: UnsafeCell<Vec<&'static mut [u8]>> = UnsafeCell::new(Vec::new());
}

/// A fast allocator for 'static byte slices for the current thread
/// (!Sync, !Send).
pub struct LeakedRegion {
    _only_on_same_thread: PhantomData<UnSyncUnSend>,
    current_region: &'static mut [u8],
}

impl Drop for LeakedRegion {
    fn drop(&mut self) {
        REGIONS.with(|regions| {
            let regions: &mut _ = unsafe { &mut *regions.get() };
            regions.push(unsafe { hack_static(self.current_region) });
        });
    }
}

impl LeakedRegion {
    // How many bytes to allocate unless the requested size was larger
    const MIN_REGION_SIZE: usize = 1048576;

    pub fn new() -> Self {
        let current_region = REGIONS.with(|regions| {
            let regions: &mut _ = unsafe { &mut *regions.get() };
            if let Some(region) = regions.pop() {
                region
            } else {
                let leaked = Self::make_region(0);
                assert!(leaked.len() > 0); // XX
                leaked
            }
        });
        Self {
            _only_on_same_thread: PhantomData,
            current_region,
        }
    }

    fn make_region(num_bytes: usize) -> &'static mut [u8] {
        let region_size = Self::MIN_REGION_SIZE.max(num_bytes);
        let mut new_region = Vec::<u8>::with_capacity(region_size);
        unsafe {
            new_region.set_len(region_size);
        }
        Box::leak(new_region.into_boxed_slice())
    }

    pub fn allocate(&mut self, num_bytes: usize) -> &'static mut [u8] {
        if num_bytes <= self.current_region.len() {
            let (res, rest) = self.current_region.split_at_mut(num_bytes);
            self.current_region = unsafe { hack_static(rest) };
            unsafe { hack_static(res) }
        } else {
            // Done with the current region.
            REGIONS.with(|regions| {
                let regions: &mut _ = unsafe { &mut *regions.get() };
                regions.push(unsafe { hack_static(self.current_region) });
            });
            self.current_region = Self::make_region(num_bytes);
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
