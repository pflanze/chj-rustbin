use std::{fmt::Display, ops::Deref};

#[derive(Debug)]
enum MyStringInner<const N: usize> {
    /// A string terminated by NUL or the end of the array
    Short([u8; N]),
    Long(Box<str>),
}

#[derive(Debug)]
pub struct MyString<const N: usize>(MyStringInner<N>);

// Todo optimization?: could rely on assumption that both sides must
// be of the same enumerant.

impl<const N: usize> PartialEq for MyString<N> {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
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
        let s: &str = t.as_ref();
        let len = s.len();
        if len <= N {
            let mut arr = [0; N];
            (&mut arr[0..len]).copy_from_slice(s.as_bytes());
            MyString(MyStringInner::Short(arr))
        } else {
            MyString(MyStringInner::Long(t.into()))
        }
    }
}

fn array_len<const N: usize>(a: &[u8; N]) -> usize {
    let mut len = 0;
    for b in a {
        if *b == 0 {
            break;
        }
        len += 1;
    }
    len
}

impl<const N: usize> AsRef<str> for MyString<N> {
    fn as_ref(&self) -> &str {
        match &self.0 {
            MyStringInner::Short(a) => {
                let len = array_len(a);
                unsafe {
                    // Safe because it was generated from a str (and
                    // we exclude the unused part)
                    std::str::from_utf8_unchecked(&a[0..len])
                }
            }
            MyStringInner::Long(b) => b.as_ref(),
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
        self.as_ref().fmt(f)
    }
}

impl<const N: usize> MyString<N> {
    pub fn is_short(&self) -> bool {
        match &self.0 {
            MyStringInner::Short(_) => true,
            MyStringInner::Long(_) => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::mem::size_of;

    use super::*;

    #[test]
    fn t_size() {
        assert_eq!(size_of::<MyString<23>>(), 24);
        assert_eq!(size_of::<MyString<24>>(), 32);
        // assert_eq!(size_of::<MyString<15>>(), 16);
        assert_eq!(size_of::<MyString<15>>(), 24);
        assert_eq!(size_of::<MyString<10>>(), 24); // bummer
        assert_eq!(size_of::<MyString<16>>(), 24);
    }
    #[test]
    fn t_() {
        let m = |s: &str| -> MyString<23> { s.into() };
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
}
