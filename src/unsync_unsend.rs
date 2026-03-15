//! Helpers to disable Send or Sync for data used with unsafe code, on stable
//!
//! If you want !Send, you need to add a marker field to your struct:
//!
//! struct Foo {
//!     _marker: PhantomData<UnSend>,
//! }
//!
//! For !Sync,
//!
//! struct Foo {
//!     _marker: PhantomData<UnSync>,
//! }
//!
//! To disable both at the same time,
//!
//! struct Foo {
//!     _marker: PhantomData<UnSyncUnSend>,
//! }

// https://stackoverflow.com/questions/62713667/how-to-implement-send-or-sync-for-a-type

#[allow(unused)]
pub struct UnSend(*const ());
unsafe impl Sync for UnSend {}

#[allow(unused)]
pub struct UnSync(*const ());
unsafe impl Send for UnSync {}

#[allow(unused)]
pub struct UnSyncUnSend(*const ());
