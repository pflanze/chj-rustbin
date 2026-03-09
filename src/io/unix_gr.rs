//! Interact with the Unix user database

use std::{collections::HashMap, ffi::CStr, pin::Pin, ptr::null_mut};

use libc::{getgrgid_r, gid_t, group};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Gid(gid_t);

impl From<gid_t> for Gid {
    fn from(v: gid_t) -> Self {
        Self(v)
    }
}

pub struct GrInfo {
    _buffer: Pin<Box<[i8]>>,
    gr: group,
}

unsafe impl Sync for GrInfo {}
unsafe impl Send for GrInfo {}

impl GrInfo {
    /// Returns Null when not unicode, but also when name is NULL (not
    /// set?)
    pub fn groupname(&self) -> Option<&str> {
        let name_ptr = self.gr.gr_name;
        if name_ptr.is_null() {
            // XX when can this happen?
            return None;
        }
        let c_str = unsafe { CStr::from_ptr(name_ptr) };
        c_str.to_str().ok()
    }
}

impl Gid {
    /// Returns None if the Gid does not exist
    pub fn gr_info(self) -> Option<GrInfo> {
        let mut gr = group {
            gr_name: null_mut(),
            gr_passwd: null_mut(),
            gr_gid: 0,
            gr_mem: null_mut(),
        };
        let pwd_ptr: *mut group = &mut gr;
        const BUFLEN: usize = 1000;
        let buf: [i8; BUFLEN] = [0; BUFLEN];
        let mut _buffer = Box::pin(buf);
        let buf_ptr: *mut i8 = (*_buffer).as_mut_ptr();

        let mut result_ptr: *mut group = null_mut();
        let result_ptr_ptr: *mut *mut group = &mut result_ptr;

        let res: i32 = unsafe {
            getgrgid_r(self.0, pwd_ptr, buf_ptr, BUFLEN, result_ptr_ptr)
        };
        if res != 0 {
            return None;
        }
        if result_ptr.is_null() {
            // when does this happen? man page is unclear.
            return None;
        }
        Some(GrInfo { gr, _buffer })
    }
}

#[derive(Default)]
pub struct GrInfoCache(HashMap<Gid, GrInfo>);

impl GrInfoCache {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn lookup_by_gid(&mut self, gid: Gid) -> Option<&GrInfo> {
        match self.0.entry(gid) {
            std::collections::hash_map::Entry::Occupied(o) => {
                Some(o.into_mut())
            }
            std::collections::hash_map::Entry::Vacant(v) => {
                if let Some(info) = gid.gr_info() {
                    Some(v.insert(info))
                } else {
                    None
                }
            }
        }
    }

    pub fn get_by_gid(&self, gid: Gid) -> Option<&GrInfo> {
        self.0.get(&gid)
    }
}
