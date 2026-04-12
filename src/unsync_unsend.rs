//! Helpers to disable Send or Sync for data used with unsafe code, on stable
//!
//! If you want !Send, you need to add a marker field to your struct:
//!
//! ```
//! use std::marker::PhantomData;
//! use chj_rustbin::unsync_unsend::UnSend;
//!
//! struct Foo {
//!     _marker: PhantomData<UnSend>,
//! }
//! ```
//!
//! For !Sync,
//!
//! ```
//! use std::marker::PhantomData;
//! use chj_rustbin::unsync_unsend::UnSync;
//!
//! struct Foo {
//!     _marker: PhantomData<UnSync>,
//! }
//! ```
//!
//! To disable both at the same time,
//!
//! ```
//! use std::marker::PhantomData;
//! use chj_rustbin::unsync_unsend::UnSyncUnSend;
//!
//! struct Foo {
//!     _marker: PhantomData<UnSyncUnSend>,
//! }
//! ```

// https://stackoverflow.com/questions/62713667/how-to-implement-send-or-sync-for-a-type

#[allow(unused)]
pub struct UnSend(*const ());
unsafe impl Sync for UnSend {}

#[allow(unused)]
pub struct UnSync(*const ());
unsafe impl Send for UnSync {}

#[allow(unused)]
pub struct UnSyncUnSend(*const ());
