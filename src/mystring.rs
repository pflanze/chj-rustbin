/// Yet another small string implementation, with more optimized space than KString offers.
// ~Just for fun.
use std::{
    cell::Cell,
    fmt::{Debug, Display},
    mem::ManuallyDrop,
    ops::Deref,
};

/// The counters wrap around
#[derive(Debug, Clone, Copy)]
pub struct Stats {
    /// Number of MyString creations that put the string inline
    pub immediate_count: u32,
    /// Number of MyString creations that put the string into a Box
    pub alloc_count: u32,
}

thread_local! {
    static STATS: Cell<Stats> = Stats { immediate_count: 0, alloc_count: 0 }.into();
}

pub fn get_stats() -> Stats {
    STATS.get()
}

union Storage<const N: usize> {
    inline: [u8; N],
    // XX should use Arc<str> instead?
    boxed: ManuallyDrop<Box<str>>,
}

const TAG_BOXED: u8 = 255;

// XX do *not* say packed, that just means pack *into* another struct is
// unaligned? But why does align(1) not lead to error or warning?
#[repr(C)]
pub struct MyString<const N: usize> {
    storage: Storage<N>,
    /// Distinguishes between inline storage and boxed, and in the first case,
    /// indicates the length (255 = boxed)
    tag: u8,
}

pub type MyString23 = MyString<23>;
pub type MyString31 = MyString<31>;
pub type MyString39 = MyString<39>;

impl<const N: usize> Clone for MyString<N> {
    fn clone(&self) -> Self {
        let Self { storage, tag } = self;
        if *tag == TAG_BOXED {
            Self {
                storage: Storage {
                    boxed: unsafe { storage.boxed.clone() },
                },
                tag: *tag,
            }
        } else {
            Self {
                storage: Storage {
                    inline: unsafe { storage.inline.clone() },
                },
                tag: *tag,
            }
        }
    }
}

impl<const N: usize> Drop for MyString<N> {
    fn drop(&mut self) {
        if self.tag == TAG_BOXED {
            unsafe {
                // Drop is guaranteed to only run once (right?)
                drop(ManuallyDrop::take(&mut self.storage.boxed));
            }
        }
    }
}

impl<const N: usize> Debug for MyString<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self.as_ref(), f)
    }
}

impl<const N: usize, const N2: usize> PartialEq<MyString<N2>> for MyString<N> {
    #[inline]
    fn eq(&self, other: &MyString<N2>) -> bool {
        if N == N2 {
            if self.tag == TAG_BOXED && other.tag == TAG_BOXED {
                unsafe {
                    // Safe as both tags indicate Inline
                    self.storage.inline.as_slice()
                        == other.storage.inline.as_slice()
                }
            } else if self.tag != TAG_BOXED && other.tag != TAG_BOXED {
                unsafe {
                    // Safe as both tags indicate Boxed
                    self.storage.boxed == other.storage.boxed
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

impl<const N: usize, T: Into<Box<str>> + AsRef<str>> From<T> for MyString<N> {
    fn from(t: T) -> Self {
        const { assert!(N < 255) }
        let s: &str = t.as_ref();
        let len = s.len();
        if len <= N {
            {
                let mut stats = STATS.get();
                stats.immediate_count = stats.immediate_count.wrapping_add(1);
                STATS.set(stats);
            }
            let mut inline = [0; N];
            (&mut inline[0..len]).copy_from_slice(s.as_bytes());
            MyString {
                storage: Storage { inline },
                tag: len as u8,
            }
        } else {
            {
                let mut stats = STATS.get();
                stats.alloc_count = stats.alloc_count.wrapping_add(1);
                STATS.set(stats);
            }
            let boxed = ManuallyDrop::new(s.into());
            MyString {
                storage: Storage { boxed },
                tag: TAG_BOXED,
            }
        }
    }
}

impl<const N: usize> AsRef<str> for MyString<N> {
    fn as_ref(&self) -> &str {
        if self.tag == TAG_BOXED {
            unsafe { &self.storage.boxed }
        } else {
            unsafe {
                std::str::from_utf8_unchecked(
                    &self.storage.inline[0..self.tag as usize],
                )
            }
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

impl<const N: usize> MyString<N> {
    #[inline]
    pub fn is_short(&self) -> bool {
        self.tag != TAG_BOXED
    }
}

#[cfg(test)]
mod tests {
    use std::mem::{size_of, size_of_val};

    use super::*;

    #[test]
    fn t_size() {
        assert_eq!(size_of::<MyString<23>>(), 24);
        assert_eq!(size_of::<MyString<24>>(), 32);
        // assert_eq!(size_of::<MyString<15>>(), 16);
        assert_eq!(size_of::<MyString<15>>(), 24);
        assert_eq!(size_of::<MyString<10>>(), 24); // bummer
        assert_eq!(size_of::<MyString<16>>(), 24);

        assert_eq!(size_of::<MyString23>(), 24);
        assert_eq!(size_of::<MyString31>(), 32);
        assert_eq!(size_of::<MyString39>(), 40);

        assert_eq!(size_of::<MyString<38>>(), 40);
        assert_eq!(size_of::<MyString<40>>(), 48);
    }

    fn m(s: &str) -> MyString<23> {
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
        let a = m("01234567890123456789012");
        let a2 = m("01234567890123456789012");
        let b = m("012345678901234567890123");
        let b2 = m("012345678901234567890123");
        assert!(a == a2);
        assert!(b == b2);
        assert!(!(a == b));

        let a = m("012345678901234567890\0\0");
        let a2 = m("012345678901234567890");
        // Currently those are in fact equal
        assert!(a == a2);
        let a = m("012345678901234567890123\0\0");
        let a2 = m("012345678901234567890123");
        // But once above the immediate size, they aren't anymore
        assert!(!(a == a2));
    }
}
