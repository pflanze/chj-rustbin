//! Interact with the Unix user database

use std::{collections::HashMap, ffi::CStr, pin::Pin, ptr::null_mut};

use libc::{getpwuid_r, passwd, uid_t};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Uid(uid_t);

impl From<uid_t> for Uid {
    fn from(v: uid_t) -> Self {
        Self(v)
    }
}

// impl TryFrom<u32> for Uid {
//     type Error = &'static str;

//     fn try_from(v: u32) -> Result<Self, Self::Error> {
//         let uid: uid_t = v.try_into().map_err(|_| "invalid uid")?;
//         Ok(Self(uid))
//     }
// }

pub struct PwInfo {
    _buffer: Pin<Box<[i8]>>,
    pwd: passwd,
}

unsafe impl Sync for PwInfo {}
unsafe impl Send for PwInfo {}

impl PwInfo {
    /// Returns Null when not unicode, but also when name is NULL (not
    /// set?)
    pub fn username(&self) -> Option<&str> {
        let name_ptr = self.pwd.pw_name;
        if name_ptr.is_null() {
            // XX when can this happen?
            return None;
        }
        let c_str = unsafe { CStr::from_ptr(name_ptr) };
        c_str.to_str().ok()
    }
}

impl Uid {
    /// Returns None if the Uid does not exist
    pub fn pw_info(self) -> Option<PwInfo> {
        let mut pwd: passwd = passwd {
            pw_name: null_mut(),
            pw_passwd: null_mut(),
            pw_uid: 0,
            pw_gid: 0,
            pw_gecos: null_mut(),
            pw_dir: null_mut(),
            pw_shell: null_mut(),
        };
        let pwd_ptr: *mut passwd = &mut pwd;
        const BUFLEN: usize = 1000;
        let buf: [i8; BUFLEN] = [0; BUFLEN];
        let mut _buffer = Box::pin(buf);
        let buf_ptr: *mut i8 = (*_buffer).as_mut_ptr();

        let mut result_ptr: *mut passwd = null_mut();
        let result_ptr_ptr: *mut *mut passwd = &mut result_ptr;

        let res: i32 = unsafe {
            getpwuid_r(self.0, pwd_ptr, buf_ptr, BUFLEN, result_ptr_ptr)
        };
        if res != 0 {
            return None;
        }
        if result_ptr.is_null() {
            // when does this happen? man page is unclear.
            return None;
        }
        Some(PwInfo { pwd, _buffer })
    }
}

#[derive(Default)]
pub struct PwInfoCache(HashMap<Uid, PwInfo>);

impl PwInfoCache {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_by_uid(&mut self, uid: Uid) -> Option<&PwInfo> {
        match self.0.entry(uid) {
            std::collections::hash_map::Entry::Occupied(o) => {
                Some(o.into_mut())
            }
            std::collections::hash_map::Entry::Vacant(v) => {
                if let Some(info) = uid.pw_info() {
                    Some(v.insert(info))
                } else {
                    None
                }
            }
        }
    }
}
