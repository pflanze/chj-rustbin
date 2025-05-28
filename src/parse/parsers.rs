use std::any::type_name;
use std::fmt::Display;
use std::str::FromStr;

use genawaiter::rc::Gen;

use crate::parse::parse_error::ParseError;

use super::parse_error::{Backing, IntoOwningBacking, StringParseContext};

#[derive(Debug, Copy, PartialEq, Eq)]
pub struct ParseableStr<'t, B: Backing> {
    pub position: usize,
    pub backing: &'t B,
    pub s: &'t str,
}

impl<'t, B: Backing> Clone for ParseableStr<'t, B> {
    fn clone(&self) -> Self {
        let ParseableStr {
            position,
            backing,
            s,
        } = self;
        Self {
            position: *position,
            backing: *backing,
            s,
        }
    }
}

impl<'t, B: Backing> ParseableStr<'t, B> {
    pub fn new(backing: &'t B) -> Self {
        Self {
            position: 0,
            backing,
            s: backing.as_ref(),
        }
    }

    /// Panics if to is before self.
    pub fn position_difference(&self, to: &Self) -> usize {
        to.position
            .checked_sub(self.position)
            .expect("to is after self")
    }

    /// Panics if to is before self.
    pub fn up_to(&self, to: &Self) -> Self {
        let len = self.position_difference(to);
        Self {
            position: self.position,
            backing: self.backing,
            s: &self.s[0..len],
        }
    }

    /// Return the end of this string, with the correct position.
    pub fn eos(&self) -> ParseableStr<'t, B> {
        ParseableStr {
            position: self.position + self.s.len(),
            backing: self.backing,
            s: "",
        }
    }

    // just Deref? vs. accidental use of extracting methods.

    pub fn is_empty(&self) -> bool {
        self.s.is_empty()
    }

    pub fn len(&self) -> usize {
        self.s.len()
    }

    pub fn first(&self) -> Option<char> {
        self.s.chars().next()
    }

    /// You usually want `drop_str` instead.
    pub fn starts_with(&self, s: &str) -> bool {
        self.s.starts_with(s)
    }
}

impl<'s, B: Backing> IntoOwningBacking<B::Owned> for ParseableStr<'s, B>
where
    &'s B: Backing,
    B::Owned: Backing,
{
    type Owning = StringParseContext<B::Owned>;

    fn into_owning_backing(self) -> Self::Owning {
        StringParseContext {
            position: self.position,
            backing: self.backing.to_owned(),
        }
    }
}

// XX hmm why both IntoOwningParseContext an From ?  Now thinking
// about From and Into instead of ToOwned and Borrow?

impl<'s, B: Backing> From<ParseableStr<'s, B>> for StringParseContext<&'s B>
where
    &'s B: Backing,
{
    fn from(value: ParseableStr<'s, B>) -> Self {
        let ParseableStr {
            position,
            backing,
            s: _,
        } = value;
        StringParseContext { position, backing }
    }
}

impl<'r, 's, B: Backing> From<&'r ParseableStr<'s, B>>
    for StringParseContext<&'s B>
where
    &'s B: Backing,
{
    fn from(value: &'r ParseableStr<'s, B>) -> Self {
        let ParseableStr {
            position,
            backing,
            s: _,
        } = *value;
        StringParseContext { position, backing }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("expected {:?}", self.needle)]
pub struct ExpectedString<'t, B: Backing> {
    pub needle: &'t str,
    pub context: ParseableStr<'t, B>,
}

#[derive(Debug, PartialEq, thiserror::Error)]
#[error("expected {}", self.desc)]
pub struct Expected<'t, B: Backing> {
    pub desc: &'t str,
    pub context: ParseableStr<'t, B>,
}

#[derive(Debug, PartialEq, thiserror::Error)]
#[error("cannot parse as {type_}: {error}")]
pub struct FromStrError {
    // Don't be crazy and get the type name in the Error trait, as
    // that would preclude passing FromStrError as dyn.
    pub type_: &'static str,
    pub error: String,
    pub position: usize,
}

impl<'t, B: Backing> From<Box<ExpectedString<'t, B>>>
    for ParseError<StringParseContext<&'t B>>
where
    &'t B: Backing,
{
    fn from(e: Box<ExpectedString<'t, B>>) -> Self {
        // Do not use parseerror! macro, since the frame here isn't
        // interesting, and could sometimes save an allocation.
        ParseError {
            message: e.to_string(),
            context: e.context.into(),
            backtrace: Vec::new(),
        }
    }
}

impl<'t, B: Backing> From<Box<Expected<'t, B>>>
    for ParseError<StringParseContext<&'t B>>
where
    &'t B: Backing,
{
    fn from(e: Box<Expected<'t, B>>) -> Self {
        // Do not use parseerror! macro, since the frame here isn't
        // interesting, and could sometimes save an allocation.
        ParseError {
            message: e.to_string(),
            context: e.context.into(),
            backtrace: Vec::new(),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ParseFailure {
    #[error("unexpected end of input")]
    Eos,
    #[error("invalid character")]
    InvalidCharacter,
}
// pub struct ParseError {
//     failure: ParseFailure,
//     position: usize
// }

impl<'t, B: Backing> From<&'t B> for ParseableStr<'t, B> {
    fn from(s: &'t B) -> Self {
        ParseableStr {
            position: 0,
            backing: s,
            s: s.as_ref(),
        }
    }
}

pub trait FromParseableStr<'s, B: Backing + 's>: Sized
where
    &'s B: Backing,
{
    fn from_parseable_str(
        s: &ParseableStr<'s, B>,
    ) -> Result<Self, ParseError<StringParseContext<&'s B>>>;
}

#[derive(Debug, Clone)]
pub struct Separator {
    /// Note: `required` does not matter if `alternatives` is empty
    /// (no separator is actually expected in that case).
    pub required: bool,
    pub alternatives: &'static [&'static str],
}

impl<'t, B: Backing> ParseableStr<'t, B> {
    /// Parse the whole string via `FromStr`, returning error information.
    pub fn parse<T: FromStr>(&self) -> Result<T, Box<FromStrError>>
    where
        T::Err: Display,
    {
        self.s.parse().map_err(|e: T::Err| {
            FromStrError {
                type_: type_name::<T>(),
                error: e.to_string(),
                position: self.position,
            }
            .into()
        })
    }

    /// Try to parse the whole string via `FromStr`.
    // try_ prefix is usually used for Result; use opt_, maybe_ ?
    pub fn opt_parse<T: FromStr>(&self) -> Option<T> {
        self.s.parse().ok()
    }

    /// Skip the given number of bytes from the beginning. Panics if
    /// num_bytes goes beyond the end of self, and will lead to later
    /// panics if the result is not pointing at a character boundary.
    pub fn skip_bytes(&self, num_bytes: usize) -> ParseableStr<'t, B> {
        let ParseableStr {
            position,
            backing,
            s,
        } = self;
        Self {
            position: position + num_bytes,
            backing,
            s: &s[num_bytes..],
        }
    }

    /// Split at the given number of bytes from the beginning. Panics if
    /// mid > len, or not pointing at a character boundary.
    pub fn split_at(
        &self,
        mid: usize,
    ) -> (ParseableStr<'t, B>, ParseableStr<'t, B>) {
        let ParseableStr {
            position,
            backing,
            s,
        } = *self;
        (
            Self {
                position,
                backing,
                s: &s[..mid],
            },
            Self {
                position: position + mid,
                backing,
                s: &s[mid..],
            },
        )
    }

    /// Trim whitespace from start end end.
    pub fn trim(&self) -> ParseableStr<'t, B> {
        let s1 = self.s.trim_start();
        Self {
            s: s1.trim_end(),
            backing: self.backing,
            position: self.position + (self.len() - s1.len()),
        }
    }

    /// Trim whitespace from the end.
    pub fn trim_start(&self) -> ParseableStr<'t, B> {
        let s1 = self.s.trim_start();
        Self {
            s: s1,
            backing: self.backing,
            position: self.position + (self.len() - s1.len()),
        }
    }

    /// Trim whitespace from the end.
    pub fn trim_end(&self) -> ParseableStr<'t, B> {
        Self {
            s: self.s.trim_end(),
            backing: self.backing,
            position: self.position,
        }
    }

    /// Find the first occurrence of `needle` in self, return
    /// the needle and what follows.
    pub fn find_str(&self, needle: &str) -> Option<ParseableStr<'t, B>> {
        let ParseableStr {
            position,
            backing,
            s,
        } = self;
        let offset = s.find(needle)?;
        Some(ParseableStr {
            position: position + offset,
            backing,
            s: &s[offset..],
        })
    }

    /// Find the first occurrence of `needle` in self, return the
    /// needle itself (with the position information of where it was
    /// found) and the rest after it.
    pub fn find_str_rest(
        &self,
        needle: &str,
    ) -> Option<(ParseableStr<'t, B>, ParseableStr<'t, B>)> {
        let ParseableStr {
            position,
            backing,
            s,
        } = self;
        let offset = s.find(needle)?;
        Some((
            ParseableStr {
                position: position + offset,
                backing,
                s: &s[offset..offset + needle.len()],
            },
            ParseableStr {
                position: position + offset + needle.len(),
                backing,
                s: &s[offset + needle.len()..],
            },
        ))
    }

    /// Find the first occurrence of `needle` in self, return
    /// the rest after it.
    pub fn after_str(&self, needle: &str) -> Option<ParseableStr<'t, B>> {
        let ParseableStr {
            position,
            backing,
            s,
        } = self;
        let pos = s.find(needle)?;
        let offset = pos + needle.len();
        Some(ParseableStr {
            position: position + offset,
            backing,
            s: &s[offset..],
        })
    }

    /// Expect `beginning` at the start of self, if so return the
    /// remainder after it.
    pub fn drop_str(&self, beginning: &str) -> Option<ParseableStr<'t, B>> {
        let ParseableStr {
            position,
            backing,
            s,
        } = *self;
        if s.starts_with(beginning) {
            Some(ParseableStr {
                position: position + beginning.len(),
                backing,
                s: &s[beginning.len()..],
            })
        } else {
            None
        }
    }

    /// Same as `drop_str` but returns an error mentioning
    /// `beginning` if it doesn't match
    pub fn expect_str(
        &self,
        beginning: &'t str,
    ) -> Result<ParseableStr<'t, B>, Box<ExpectedString<'t, B>>> {
        let ParseableStr {
            position,
            backing,
            s,
        } = *self;
        self.drop_str(beginning).ok_or_else(|| {
            ExpectedString {
                needle: beginning,
                context: ParseableStr {
                    position,
                    backing,
                    s,
                },
            }
            .into()
        })
    }

    pub fn expect_str_or_eos(
        &self,
        beginning: &'t str,
    ) -> Result<ParseableStr<'t, B>, Box<ExpectedString<'t, B>>> {
        if self.is_empty() {
            return Ok(self.clone());
        }
        self.expect_str(beginning)
    }

    // ExpectedString error carries the strings from the Separator
    // which are 'static, but it also contains self now, thus lifetime
    // 't.
    pub fn expect_separator(
        &self,
        separator: &Separator,
    ) -> Result<ParseableStr<'t, B>, Box<ExpectedString<'t, B>>> {
        let mut last_e = None;
        for &sep in separator.alternatives {
            match self.expect_str(sep) {
                Ok(s) => return Ok(s),
                Err(e) => last_e = Some(e),
            }
        }
        if separator.required {
            if let Some(last_e) = last_e {
                // XX again should 'merge' error messages instead
                Err(last_e)
            } else {
                Ok(self.clone())
            }
        } else {
            Ok(self.clone())
        }
    }

    /// Find the first occurrence of `needle` in self, return the part
    /// left of it.
    pub fn take_until_str(&self, needle: &str) -> Option<ParseableStr<'t, B>> {
        let ParseableStr {
            position,
            backing,
            s,
        } = *self;
        let pos = s.find(needle)?;
        Some(ParseableStr {
            position,
            backing,
            s: &s[0..pos],
        })
    }

    /// Split at the first occurrence of `needle`, returning the parts
    /// before and after it.
    pub fn split_at_str(
        &self,
        needle: &str,
    ) -> Option<(ParseableStr<'t, B>, ParseableStr<'t, B>)> {
        let ParseableStr {
            position,
            backing,
            s,
        } = *self;
        let pos = s.find(needle)?;
        Some((
            ParseableStr {
                position,
                backing,
                s: &s[0..pos],
            },
            ParseableStr {
                position: position + pos + needle.len(),
                backing,
                s: &s[pos + needle.len()..],
            },
        ))
    }

    /// Take every character for which `pred` returns true, return the
    /// string making up those characters and the remainder.
    pub fn take_while(
        &self,
        mut pred: impl FnMut(char) -> bool,
    ) -> (ParseableStr<'t, B>, ParseableStr<'t, B>) {
        let ParseableStr {
            position,
            backing,
            s,
        } = *self;
        for (pos, c) in s.char_indices() {
            if !pred(c) {
                return (
                    ParseableStr {
                        position,
                        backing,
                        s: &s[0..pos],
                    },
                    ParseableStr {
                        position: position + pos,
                        backing,
                        s: &s[pos..],
                    },
                );
            }
        }
        (
            self.clone(),
            ParseableStr {
                s: "",
                position: position + s.len(),
                backing,
            },
        )
    }

    /// Expect 1 character for which `pred` must return true, return
    /// the string making up the remainder.
    pub fn expect1_matching(
        &self,
        pred: impl FnOnce(char) -> bool,
        desc: &'t str,
    ) -> Result<ParseableStr<'t, B>, Box<Expected<'t, B>>> {
        let ParseableStr {
            position,
            backing,
            s,
        } = self;
        let err = || {
            Err(Expected {
                desc,
                context: self.clone(),
            }
            .into())
        };
        if s.is_empty() {
            return err();
        }
        let mut cs = s.char_indices();
        let (_, c) = cs.next().unwrap();
        if !pred(c) {
            return err();
        }
        let (pos, _) = cs.next().unwrap_or_else(|| (s.len(), ' '));
        Ok(ParseableStr {
            position: position + pos,
            backing,
            s: &s[pos..],
        })
    }

    pub fn take_identifier(
        &self,
    ) -> Result<(ParseableStr<'t, B>, ParseableStr<'t, B>), Box<Expected<'t, B>>>
    {
        let rest = self.expect1_matching(
            |c| c.is_ascii_lowercase() && (c.is_ascii_alphabetic() || c == '_'),
            "[a-z_] followed by [a-z0-9_]*",
        )?;
        let rest = rest.drop_while(|c| {
            c.is_ascii_lowercase() && (c.is_ascii_alphanumeric() || c == '_')
        });
        let ParseableStr {
            position,
            backing,
            s,
        } = *self;
        Ok((
            ParseableStr {
                position,
                backing,
                s: &s[0..(rest.position - position)],
            },
            rest,
        ))
    }

    pub fn drop_while(
        &self,
        mut pred: impl FnMut(char) -> bool,
    ) -> ParseableStr<'t, B> {
        let ParseableStr {
            position,
            backing,
            s,
        } = self;
        for (pos, c) in s.char_indices() {
            if !pred(c) {
                return ParseableStr {
                    position: position + pos,
                    backing,
                    s: &s[pos..],
                };
            }
        }
        ParseableStr {
            position: position + s.len(),
            backing,
            s: "",
        }
    }

    /// Take `n` characters matching `pred`. Does not check whether
    /// there are more than `n` characters matching `pred`, just
    /// returns what was found so far.
    pub fn take_n_while(
        &self,
        n: usize,
        pred: impl FnMut(char) -> bool,
        desc: &'t str,
    ) -> Result<(ParseableStr<'t, B>, ParseableStr<'t, B>), Box<Expected<'t, B>>>
    {
        self.take_nrange_while(n, n, pred, desc)
    }

    /// Take `n_min` to `n_max` (inclusive) characters matching
    /// `pred`. Does not check whether there are more than `n_max`
    /// characters matching `pred`, just returns what was found so
    /// far.
    pub fn take_nrange_while(
        &self,
        n_min: usize,
        n_max: usize, // inclusive
        mut pred: impl FnMut(char) -> bool,
        desc: &'t str,
    ) -> Result<(ParseableStr<'t, B>, ParseableStr<'t, B>), Box<Expected<'t, B>>>
    {
        let ParseableStr {
            position,
            backing,
            s,
        } = *self;
        let mut cs = s.char_indices();
        let mut failed_pos = None;
        for i in 0..n_max {
            if let Some((pos, c)) = cs.next() {
                if !pred(c) {
                    if i < n_min {
                        // Should it report the begin of the n characters
                        // or the character failing? Or use Cow and
                        // generate a new message saying where the begin
                        // is? Or use another Expected type that carries
                        // the info, rather.
                        return Err(Expected {
                            desc,
                            context: ParseableStr {
                                s,
                                backing,
                                position: position + pos,
                            },
                        }
                        .into());
                    }
                    failed_pos = Some(pos);
                    break;
                }
            } else {
                // Eos
                if i >= n_min {
                    failed_pos = Some(s.len());
                    break;
                } else {
                    return Err(Expected {
                        desc,
                        context: ParseableStr {
                            s,
                            backing,
                            position: position + s.len(),
                        },
                    }
                    .into());
                }
            }
        }
        let pos;
        if let Some(failed_pos) = failed_pos {
            pos = failed_pos;
        } else {
            (pos, _) = cs.next().unwrap_or_else(|| (s.len(), ' '));
        }
        Ok((
            ParseableStr {
                position,
                backing,
                s: &s[0..pos],
            },
            ParseableStr {
                position: position + pos,
                backing,
                s: &s[pos..],
            },
        ))
    }

    pub fn drop_whitespace(&self) -> ParseableStr<'t, B> {
        self.drop_while(|c| c.is_whitespace())
    }

    /// Split on a fixed string, `separator`; the `separator` string
    /// is not included in the returned parts. If
    /// `omit_empty_last_item` is true, if there's an empty string
    /// after the last separator, it is not reported as an item.
    pub fn split_str<'r, 'n>(
        &'r self,
        separator: &'n str,
        omit_empty_last_item: bool,
    ) -> Box<dyn Iterator<Item = ParseableStr<'t, B>> + 'n>
    where
        't: 'n,
        'r: 'n,
    {
        Box::new(
            Gen::new(|co| async move {
                let mut rest = self.clone();
                loop {
                    if omit_empty_last_item && rest.is_empty() {
                        break;
                    }
                    if let Some(p2) = rest.find_str(separator) {
                        co.yield_(rest.up_to(&p2)).await;
                        rest = p2.skip_bytes(separator.len());
                    } else {
                        co.yield_(rest).await;
                        break;
                    }
                }
            })
            .into_iter(),
        )
    }
}

// pub fn expect_alternatives_2(
//     s: ParseableStr,
//     a: impl FnOnce(ParseableStr) -> Result<ParseableStr, ParseError>,
//     b: impl FnOnce(ParseableStr) -> Result<ParseableStr, ParseError>,
// ) -> Result<ParseableStr, ParseError> {
//     match a(s) {
//         Ok(s) => Ok(s),
//         Err(e1) => match b(s) {
//             Ok(s) => Ok(s),
//             // Return the first error. Merging the errors wouldn't
//             // really work because of the single position our api
//             // wants to return (sigh).
//             Err(_) => Err(e1)
//         }
//     }
// }

#[cfg(test)]
mod tests {
    use super::*;

    // Oh my; should ParseableStr be able to own the backing, too? But
    // that will be ugly to implement in multiple ways.
    macro_rules! let_parseable {
        { $backingvar:ident, $var:ident = $backing:expr } => {
            let $backingvar: String = $backing.into();
            let $var = ParseableStr::from(&$backingvar);
        }
    }
    macro_rules! new {
        { $backingvar:ident, $pos:expr, $string:expr } => {
            ParseableStr {
                position: $pos,
                backing: &$backingvar,
                s: $string
            }
        }
    }

    #[test]
    fn t_split_at_str() {
        let_parseable! { backing, s = "foo -- bar" }
        assert_eq!(s.split_at_str("---"), None);

        let_parseable! { backing, s = "foo -- bar" }
        assert_eq!(
            s.split_at_str("--"),
            Some((
                ParseableStr {
                    s: "foo ",
                    backing: &backing,
                    position: 0
                },
                ParseableStr {
                    s: " bar",
                    backing: &backing,
                    position: 6
                },
            ))
        );
    }

    #[test]
    fn t_split_str() {
        {
            let_parseable! { backing, s = "foo - bar- baz -  bam" }
            let r1: Vec<ParseableStr<_>> = s.split_str(" - ", false).collect();
            assert_eq!(
                &*r1,
                &[
                    new!(backing, 0, "foo"),
                    new!(backing, 6, "bar- baz"),
                    new!(backing, 17, " bam")
                ]
            );
        }
        {
            let_parseable! { backing, s = "foo - bar- baz - " }
            let r1: Vec<ParseableStr<_>> = s.split_str(" - ", false).collect();
            assert_eq!(
                &*r1,
                &[
                    new!(backing, 0, "foo"),
                    new!(backing, 6, "bar- baz"),
                    new!(backing, 17, "")
                ]
            );
        }
        {
            let_parseable! { backing, s = "foo - bar- baz - " }
            let r1: Vec<ParseableStr<_>> = s.split_str(" - ", true).collect();
            assert_eq!(
                &*r1,
                &[new!(backing, 0, "foo"), new!(backing, 6, "bar- baz"),]
            );
        }
        {
            let_parseable! { backing, s = " - " }
            let r1: Vec<ParseableStr<_>> = s.split_str(" - ", false).collect();
            assert_eq!(&*r1, &[new!(backing, 0, ""), new!(backing, 3, ""),]);
        }
        {
            let_parseable! { backing, s = " - " }
            let r1: Vec<ParseableStr<_>> = s.split_str(" - ", true).collect();
            assert_eq!(&*r1, &[new!(backing, 0, ""),]);
        }
        {
            let_parseable! { backing, s = "" }
            let r1: Vec<ParseableStr<_>> = s.split_str(" - ", false).collect();
            assert_eq!(&*r1, &[new!(backing, 0, ""),]);
        }
        {
            let_parseable! { backing, s = "" }
            let r1: Vec<ParseableStr<_>> = s.split_str(" - ", true).collect();
            assert_eq!(&*r1, &[]);
        }
    }

    #[test]
    fn t_take_while() {
        let_parseable! { backing, s = "" }
        assert_eq!(
            s.take_while(|c| c.is_alphanumeric()),
            (new!(backing, 0, ""), new!(backing, 0, ""),)
        );

        let_parseable! { backing, s = " abc def" }
        assert_eq!(
            s.take_while(|c| c.is_alphanumeric()),
            (new!(backing, 0, ""), new!(backing, 0, " abc def"),)
        );

        let_parseable! { backing, s = "abc def" }
        assert_eq!(
            s.take_while(|c| c.is_alphanumeric()),
            (new!(backing, 0, "abc"), new!(backing, 3, " def"),)
        );

        let_parseable! { backing, s = "abcdef" }
        assert_eq!(
            s.take_while(|c| c.is_alphanumeric()),
            (new!(backing, 0, "abcdef"), new!(backing, 6, ""),)
        );
    }

    #[test]
    fn t_take_n_while() {
        fn t<'s, 't>(
            s: &'s ParseableStr<'t, String>,
            n: usize,
        ) -> Result<
            (ParseableStr<'t, String>, ParseableStr<'t, String>),
            Box<Expected<'t, String>>,
        > {
            s.take_n_while(
                n,
                |c| c.is_ascii_digit(),
                "digit as part of year number",
            )
        }

        fn ok<'s, 't>(
            backing: &'t String,
            s0: &'t str,
            position: usize,
            s1: &'t str,
        ) -> Result<
            (ParseableStr<'t, String>, ParseableStr<'t, String>),
            Box<Expected<'t, String>>,
        > {
            Ok((
                ParseableStr {
                    position: 0,
                    backing,
                    s: s0,
                },
                ParseableStr {
                    position,
                    backing,
                    s: s1,
                },
            ))
        }

        let err = |backing, position, desc, s| {
            Err(Box::new(Expected {
                desc,
                context: ParseableStr {
                    position,
                    backing,
                    s,
                },
            }))
        };

        macro_rules! test {
            { t($sourcestr:expr, $n:expr), ok($matchstr:expr, $matchpos:expr, $reststr:expr) } => {
                let_parseable!{ backing, s = $sourcestr }
                assert_eq!(t(&s, $n), ok(&backing, $matchstr, $matchpos, $reststr));
            };
            { t($sourcestr:expr, $n:expr), err($pos:expr, $msg:expr, $matchstr:expr) } => {
                let_parseable!{ backing, s = $sourcestr }
                assert_eq!(t(&s, $n), err(&backing, $pos, $msg, $matchstr));
            }
        }

        test!(t("2024-10", 4), ok("2024", 4, "-10"));
        test!(t("2024-10", 3), ok("202", 3, "4-10"));
        test!(t("2024-10", 0), ok("", 0, "2024-10"));
        // XX in these err tests: is it correct to carry the whole string in `s`?:
        test!(
            t("2024-10", 5),
            err(4, "digit as part of year number", "2024-10")
        );
        test!(t("2024", 5), err(4, "digit as part of year number", "2024"));
        test!(t("2024", 4), ok("2024", 4, ""));
    }

    #[test]
    fn t_take_nrange_while() {
        fn t<'s, 't>(
            s: &ParseableStr<'t, String>,
            n_min: usize,
            n_max: usize,
        ) -> Result<
            (ParseableStr<'t, String>, ParseableStr<'t, String>),
            Box<Expected<'t, String>>,
        > {
            s.take_nrange_while(
                n_min,
                n_max,
                |c| c.is_ascii_digit(),
                "digit as part of year number",
            )
        }

        fn ok<'s, 't>(
            backing: &'t String,
            s0: &'t str,
            position: usize,
            s1: &'t str,
        ) -> Result<
            (ParseableStr<'t, String>, ParseableStr<'t, String>),
            Box<Expected<'t, String>>,
        > {
            Ok((
                ParseableStr {
                    position: 0,
                    backing,
                    s: s0,
                },
                ParseableStr {
                    position,
                    backing,
                    s: s1,
                },
            ))
        }

        let err = |backing, position, desc, s| {
            Err(Box::new(Expected {
                desc,
                context: ParseableStr {
                    position,
                    backing,
                    s,
                },
            }))
        };

        macro_rules! test {
            { t($sourcestr:expr, $n_min:expr, $n_max:expr), ok($matchstr:expr, $matchpos:expr, $reststr:expr) } => {
                let_parseable!{ backing, s = $sourcestr }
                assert_eq!(t(&s, $n_min, $n_max), ok(&backing, $matchstr, $matchpos, $reststr));
            };
            { t($sourcestr:expr, $n_min:expr, $n_max:expr), err($pos:expr, $msg:expr, $matchstr:expr) } => {
                let_parseable!{ backing, s = $sourcestr }
                assert_eq!(t(&s, $n_min, $n_max), err(&backing, $pos, $msg, $matchstr));
            }
        }

        test!(t("2024-10", 3, 4), ok("2024", 4, "-10"));
        test!(t("2024-10", 1, 3), ok("202", 3, "4-10"));
        test!(t("2024-10", 8, 3), ok("202", 3, "4-10"));
        test!(t("2024-10", 3, 0), ok("", 0, "2024-10"));
        test!(t("2024-10", 4, 5), ok("2024", 4, "-10"));
        // XX is it correct to carry the whole string in `s`?:
        test!(
            t("2024-10 abc", 5, 6),
            err(4, "digit as part of year number", "2024-10 abc")
        );
        test!(t("2024", 3, 4), ok("2024", 4, ""));
        test!(t("2024x", 4, 5), ok("2024", 4, "x"));
        test!(t("2024", 4, 5), ok("2024", 4, ""));
    }
}
