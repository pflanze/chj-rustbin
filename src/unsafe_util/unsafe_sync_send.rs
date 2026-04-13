#[repr(transparent)]
pub struct UnsafeSync<T>(T);

unsafe impl<T> Sync for UnsafeSync<T> {}

impl<T> UnsafeSync<T> {
    pub fn new(val: T) -> Self {
        Self(val)
    }

    /// Only safe to call on the original thread
    pub unsafe fn deref(&self) -> &T {
        &self.0
    }

    /// Only safe to call on the original thread
    pub unsafe fn deref_mut(&mut self) -> &mut T {
        &mut self.0
    }

    /// Only safe to call on the original thread
    pub unsafe fn into_inner(self) -> T {
        self.0
    }
}

#[repr(transparent)]
pub struct UnsafeSend<T>(T);

unsafe impl<T> Send for UnsafeSend<T> {}

impl<T> UnsafeSend<T> {
    pub fn new(val: T) -> Self {
        Self(val)
    }

    /// Only safe to call on the original thread
    pub unsafe fn deref(&self) -> &T {
        &self.0
    }

    /// Only safe to call on the original thread
    pub unsafe fn deref_mut(&mut self) -> &mut T {
        &mut self.0
    }

    /// Only safe to call on the original thread
    pub unsafe fn into_inner(self) -> T {
        self.0
    }
}

#[repr(transparent)]
pub struct UnsafeSyncSend<T>(T);

unsafe impl<T> Sync for UnsafeSyncSend<T> {}
unsafe impl<T> Send for UnsafeSyncSend<T> {}

impl<T> UnsafeSyncSend<T> {
    pub fn new(val: T) -> Self {
        Self(val)
    }

    /// Only safe to call on the original thread
    pub unsafe fn deref(&self) -> &T {
        &self.0
    }

    /// Only safe to call on the original thread
    pub unsafe fn deref_mut(&mut self) -> &mut T {
        &mut self.0
    }

    /// Only safe to call on the original thread
    pub unsafe fn into_inner(self) -> T {
        self.0
    }
}
