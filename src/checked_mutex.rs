//! Check whether the thread itself already locks the mutex; only a
//! *hack* currently, won't be generally reliable, right? Should
//! probably use cooptex or tracing-mutex instead.

//! To use conditionally in debug mode, import as:
//!
//!     #[cfg(debug_assertions)]
//!     use chj_rustbin::checked_mutex::{CheckedMutex as Mutex, CheckedMutexGuard as MutexGuard};
//!     #[cfg(not(debug_assertions))]
//!     use std::sync::{Mutex, MutexGuard};
//!

use std::{
    ops::{Deref, DerefMut},
    sync::{Mutex, MutexGuard},
    thread::ThreadId,
};

#[derive(Debug, thiserror::Error)]
pub enum CheckedMutexError {
    // #[error("mutex is poisoned: {0}")]
    // PoisonError(PoisonError<MutexGuard<'m, T>>),
    #[error("mutex is poisoned")]
    PoisonError,
    #[error("mutex is locked by the same thread already")]
    LockedByOurselves,
}

#[derive(Debug)]
pub struct CheckedMutex<T> {
    locked_by: Mutex<Option<ThreadId>>,
    mutex: Mutex<T>,
}

pub struct CheckedMutexGuard<'m, T> {
    checked_mutex: &'m CheckedMutex<T>,
    guard: MutexGuard<'m, T>,
}

impl<'m, T> Deref for CheckedMutexGuard<'m, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.guard.deref()
    }
}

impl<'m, T> DerefMut for CheckedMutexGuard<'m, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.guard.deref_mut()
    }
}

impl<'m, T> Drop for CheckedMutexGuard<'m, T> {
    fn drop(&mut self) {
        let mut locked_by = self.checked_mutex.locked_by.lock().unwrap();
        // XX should we check if it's still us in there? Or is it
        // guaranteed? Does Drop only run for self.guard *after* this
        // code? If so it should be guaranteed, right?
        *locked_by = None;
    }
}

impl<T> CheckedMutex<T> {
    pub fn new(value: T) -> Self {
        Self {
            locked_by: Mutex::new(None),
            mutex: Mutex::new(value),
        }
    }

    pub fn lock(&self) -> Result<CheckedMutexGuard<T>, CheckedMutexError> {
        // The idea is to lock `locked_by`, if us then give error, if
        // not, get lock on `mutex`, when gotten, enter us into
        // `locked_by` and unlock that field. On dropping
        // CheckedMutexGuard, delete us if it's still us (can it be
        // another thread? Actually can't right?)
        let mut locked_by = self.locked_by.lock().unwrap();
        let id = std::thread::current().id();
        if let Some(locking_id) = *locked_by {
            if locking_id == id {
                return Err(CheckedMutexError::LockedByOurselves);
            }
        }
        match self.mutex.lock() {
            Ok(guard) => {
                // Got the lock, enter ourselves into locked_by
                *locked_by = Some(id);
                Ok(CheckedMutexGuard {
                    checked_mutex: self,
                    guard,
                })
            }
            Err(_e) => Err(CheckedMutexError::PoisonError),
        }
    }
}
