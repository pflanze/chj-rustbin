/// Yet another small string implementation, with more optimized space than KString offers.
// ~Just for fun.
use std::{
    cell::Cell,
    fmt::{Debug, Display},
    hash::Hash,
    mem::{transmute, ManuallyDrop},
    ops::Deref,
    sync::Arc,
};

/// The counters wrap around
#[derive(Debug, Clone, Copy)]
pub struct Stats {
    /// Number of MyString creations that put the string inline
    pub immediate_count: u32,
    /// Number of MyString creations that put the string into an Arc<str>
    pub alloc_count: u32,
}

thread_local! {
    static STATS: Cell<Stats> = Stats { immediate_count: 0, alloc_count: 0 }.into();
}

pub fn get_stats() -> Stats {
    STATS.get()
}

const TAG_BOXED: u8 = 255;

#[repr(C)]
union Storage<const N: usize> {
    inline: [u8; N],
    /// The u8 is only there to make really sure that there is a byte
    /// of space left after the Arc, even though it is always accessed
    /// via `.inline`, just so that the Arc is not sharing space with
    /// the tag
    boxed: (ManuallyDrop<Arc<str>>, u8),
}

// XX see Git history for previous attempts
#[repr(align(8))]
pub struct MyString<const N: usize> {
    storage: Storage<N>,
}

pub type MyString24 = MyString<24>;
pub type MyString32 = MyString<32>;
pub type MyString40 = MyString<40>;

impl<const N: usize> MyString<N> {
    #[inline]
    fn tag(&self) -> u8 {
        unsafe {
            // The other union field is never written after the
            // initial write and the tag does not overlap with the
            // payload there, thus can always read it from the
            // `.inline` field
            self.storage.inline[N - 1]
        }
    }

    /// Only defined if `self.tag() == TAG_BOXED`
    unsafe fn boxed(&self) -> &Arc<str> {
        &self.storage.boxed.0
    }

    /// Only defined if `self.tag() == TAG_BOXED`
    unsafe fn boxed_mut(&mut self) -> &mut ManuallyDrop<Arc<str>> {
        &mut self.storage.boxed.0
    }

    /// Only defined if `self.tag() != TAG_BOXED` and the tag value is passed as len
    unsafe fn inline(&self, len: u8) -> &str {
        unsafe {
            std::str::from_utf8_unchecked(&self.storage.inline[0..len as usize])
        }
    }

    #[inline]
    pub fn is_short(&self) -> bool {
        self.tag() != TAG_BOXED
    }
}

impl<const N: usize> Clone for MyString<N> {
    fn clone(&self) -> Self {
        if self.tag() == TAG_BOXED {
            let mut storage = Storage {
                boxed: (ManuallyDrop::new(unsafe { self.boxed() }.clone()), 0),
            };
            unsafe {
                // Safe because we make sure that .boxed.0 does not
                // overlap
                storage.inline[N - 1] = TAG_BOXED;
            }
            Self { storage }
        } else {
            Self {
                storage: Storage {
                    inline: unsafe { self.storage.inline },
                },
            }
        }
    }
}

impl<const N: usize> Drop for MyString<N> {
    fn drop(&mut self) {
        if self.tag() == TAG_BOXED {
            unsafe {
                // Drop is guaranteed to only run once (right?)
                drop(ManuallyDrop::take(self.boxed_mut()));
            }
        }
    }
}

impl<const N: usize> Debug for MyString<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self.as_ref(), f)
    }
}

impl<const N: usize> Hash for MyString<N> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state);
    }
}

impl<const N: usize, const N2: usize> PartialEq<MyString<N2>> for MyString<N> {
    #[inline]
    fn eq(&self, other: &MyString<N2>) -> bool {
        if N == N2 {
            let t1 = self.tag();
            let t2 = other.tag();
            if t1 == TAG_BOXED && t2 == TAG_BOXED {
                unsafe {
                    // Safe as both tags indicate boxed
                    self.boxed() == other.boxed()
                }
            } else if t1 != TAG_BOXED && t2 != TAG_BOXED {
                // Check tag/length as part of the full storage
                // check. (Prefixing with `t1 == t2 &&` seems slower.)
                unsafe {
                    // - Transmute is safe because N == N2 as checked above.
                    &self.storage.inline
                        == transmute::<&[u8; N2], &[u8; N]>(
                            &other.storage.inline,
                        )
                }
            } else {
                false
            }
        } else {
            self.as_ref() == other.as_ref()
        }
    }
}

impl<const N: usize> Eq for MyString<N> {}

impl<const N: usize> Ord for MyString<N> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_ref().cmp(other.as_ref())
    }
}

impl<const N: usize> PartialOrd for MyString<N> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<const N: usize, T: Into<Arc<str>> + AsRef<str>> From<T> for MyString<N> {
    fn from(t: T) -> Self {
        const {
            assert!(N <= 255); // len up to 254, last byte for the tag
            assert!(N > size_of::<Arc<str>>())
        }
        let s: &str = t.as_ref();
        let len = s.len();
        if len < N {
            // {
            //     let mut stats = STATS.get();
            //     stats.immediate_count = stats.immediate_count.wrapping_add(1);
            //     STATS.set(stats);
            // }
            let mut inline = [0; N];
            (&mut inline[0..len]).copy_from_slice(s.as_bytes());
            inline[N - 1] = len as u8;
            MyString {
                storage: Storage { inline },
            }
        } else {
            // {
            //     let mut stats = STATS.get();
            //     stats.alloc_count = stats.alloc_count.wrapping_add(1);
            //     STATS.set(stats);
            // }
            let mut storage = Storage { inline: [0u8; N] };
            storage.boxed = (ManuallyDrop::new(s.into()), 0);
            unsafe {
                // Safe because we make sure that .boxed.0 does not
                // overlap
                storage.inline[N - 1] = TAG_BOXED;
            }
            MyString { storage }
        }
    }
}

impl<const N: usize> AsRef<str> for MyString<N> {
    fn as_ref(&self) -> &str {
        let t = self.tag();
        if t == TAG_BOXED {
            unsafe { self.boxed() }
        } else {
            unsafe { self.inline(t) }
        }
    }
}

impl<const N: usize> Deref for MyString<N> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<const N: usize> Display for MyString<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_ref(), f)
    }
}

#[cfg(test)]
mod tests {
    use std::mem::{size_of, size_of_val};

    use super::*;

    #[test]
    fn t_size() {
        assert_eq!(size_of::<MyString<23>>(), 24);
        assert_eq!(size_of::<MyString<24>>(), 24);
        assert_eq!(size_of::<MyString<25>>(), 32);
        _ = MyString::<24>::from("foo");
        // _ = MyString::<16>::from("foo"); -- assertion failed: N > size_of::<Arc<str>>()
        _ = MyString::<17>::from("foo");
        // The type can still be used, though. Bummer, can I put a
        // const panic somewhere to disallow this?
        assert_eq!(size_of::<MyString<16>>(), 24);

        assert_eq!(size_of::<MyString24>(), 24);
        assert_eq!(size_of::<MyString32>(), 32);
        assert_eq!(size_of::<MyString40>(), 40);
    }

    fn m(s: &str) -> MyString<24> {
        s.into()
    }

    #[test]
    fn t_correctness() {
        let s = "foo";
        let ms = m(s);
        assert_eq!(&*ms, s);
        assert!(ms.is_short());

        let s = "hellö wörld";
        let ms = m(s);
        assert_eq!(&*ms, s);
        assert!(ms.is_short());

        let s = "01234567890123456789012";
        let ms = m(s);
        assert_eq!(&*ms, s);
        assert!(ms.is_short());

        let s = "012345678901234567890123";
        let ms = m(s);
        assert_eq!(&*ms, s);
        assert!(!ms.is_short());
    }

    #[test]
    fn t_kstring() {
        // Verify my claim that MyString offers more inline space than
        // KString.
        use kstring::KString;

        let s = "01234567890123456789012";
        let ms = m(s);
        assert_eq!(&*ms, s);
        assert!(ms.is_short());

        fn is_inline(ks: &KString) -> bool {
            let s = format!("{ks:?}");
            s.contains("Inline")
        }

        let ks = KString::from_ref(s);
        assert!(!is_inline(&ks));

        // vs.                               "01234567890123456789012" // 23
        assert!(is_inline(&KString::from_ref("012345678901234"))); // 15
        assert!(!is_inline(&KString::from_ref("0123456789012345")));

        assert_eq!(size_of_val(&ms), size_of_val(&ks));
    }

    #[test]
    fn t_eq() {
        // 23 -> inline
        let a = m("01234567890123456789012");
        let a2 = m("01234567890123456789012");
        // 24 -> boxed
        let b = m("012345678901234567890123");
        let b2 = m("012345678901234567890123");
        assert_eq!(a, a2);
        assert_eq!(b, b2);
        assert!(!(a == b));

        let a = m("012345678901234567890\0\0");
        let a2 = m("012345678901234567890");
        assert!(!(a == a2));
        let a = m("012345678901234567890123\0\0");
        let a2 = m("012345678901234567890123");
        assert!(!(a == a2));
    }
}
