use genawaiter::rc::Gen;

use crate::parse_error::ParseError;
use crate::parse_error;


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ParseableStr<'t> {
    pub position: usize,
    pub s: &'t str,
}

impl<'t> ParseableStr<'t> {
    pub fn new(s: &'t str) -> Self {
        Self {
            position: 0,
            s
        }
    }

    /// Panics if to is before self.
    pub fn position_difference(&self, to: ParseableStr) -> usize {
        to.position.checked_sub(self.position).expect("to is after self")
    }

    /// Panics if to is before self.
    pub fn up_to(&self, to: ParseableStr) -> Self {
        let len = self.position_difference(to);
        Self {
            position: self.position,
            s: &self.s[0..len]
        }
    }

    // just Deref? vs. accidental use of extracting methods.

    pub fn is_empty(&self) -> bool {
        self.s.is_empty()
    }

    pub fn len(&self) -> usize {
        self.s.len()
    }
    
}

#[derive(Debug, thiserror::Error)]
#[error("expected {:?}", self.needle)]
pub struct ExpectedString<'t> {
    pub needle: &'t str,
    pub position: usize
}

#[derive(Debug, PartialEq, thiserror::Error)]
#[error("expected {}", self.desc)]
pub struct Expected<'d> {
    pub desc: &'d str,
    pub position: usize
}

impl<'t> From<Box<ExpectedString<'t>>> for ParseError {
    fn from(e: Box<ExpectedString>) -> Self {
        parse_error! {
            message: e.to_string(),
            position: e.position
        }
    }
}

impl<'t> From<Box<Expected<'t>>> for ParseError {
    fn from(e: Box<Expected>) -> Self {
        parse_error! {
            message: e.to_string(),
            position: e.position
        }
    }
}



#[derive(Debug, thiserror::Error)]
pub enum ParseFailure {
    #[error("unexpected end of input")]
    Eof,
    #[error("invalid character")]
    InvalidCharacter,
}
// pub struct ParseError {
//     failure: ParseFailure,
//     position: usize
// }

#[extension(pub trait IntoParseable)]
impl<'t> &'t str {
    fn into_parseable(self) -> ParseableStr<'t> {
        ParseableStr { position: 0, s: self }
    }
}

impl<'t> From<&'t str> for ParseableStr<'t> {
    fn from(s: &'t str) -> Self {
        ParseableStr { position: 0, s }
    }
}

pub trait FromParseableStr: Sized {
    type Err;
    fn from_parseable_str(s: ParseableStr) -> Result<Self, Self::Err>;
}


#[extension(pub trait Parseable)]
impl<'t> ParseableStr<'t> {
    /// Skip the given number of bytes from the beginning. Panics if
    /// num_bytes goes beyond the end of self, and will lead to later
    /// panics if the result is not pointing at a character boundary.
    fn skip_bytes(self, num_bytes: usize) -> ParseableStr<'t> {
        let ParseableStr { position, s } = self;
        Self {
            position: position + num_bytes,
            s: &s[num_bytes..]
        }
    }

    /// Trim whitespace on both ends
    fn trim(self) -> ParseableStr<'t> {
        let s1 = self.s.trim_start();
        Self {
            s: s1.trim_end(),
            position: self.position + (self.len() - s1.len())
        }
    }

    /// Find the first occurrence of `needle` in self, return
    /// the needle and what follows.
    fn find_str(self, needle: &str) -> Option<ParseableStr<'t>> {
        let ParseableStr { position, s } = self;
        let offset = s.find(needle)?;
        Some(ParseableStr {
            position: position + offset,
            s: &s[offset..]
        })
    }

    /// Find the first occurrence of `needle` in self, return
    /// the rest after it.
    fn after_str(self, needle: &str) -> Option<ParseableStr<'t>> {
        let ParseableStr { position, s } = self;
        let pos = s.find(needle)?;
        let offset = pos + needle.len();
        Some(ParseableStr {
            position: position + offset,
            s: &s[offset..]
        })
    }

    /// Expect `beginning` at the start of self, if so return the
    /// remainder after it.
    fn drop_str(self, beginning: &str) -> Option<ParseableStr<'t>> {
        let ParseableStr { position, s } = self;
        if s.starts_with(beginning) {
            Some(ParseableStr {
                position: position + beginning.len(),
                s: &s[beginning.len()..],
            })
        } else {
            None
        }
    }

    /// Same as `drop_str` but returns an error mentioning
    /// `beginning` if it doesn't match
    fn expect_str<'needle>(self, beginning: &'needle str)
                           -> Result<ParseableStr<'t>, Box<ExpectedString<'needle>>> {
        self.drop_str(beginning).ok_or_else(
            || ExpectedString {
                needle: beginning, position: self.position
            }.into())
    }

    /// Find the first occurrence of `needle` in self, return the part
    /// left of it.
    fn take_until_str(self, needle: &str) -> Option<ParseableStr<'t>> {
        let ParseableStr { position, s } = self;
        let pos = s.find(needle)?;
        Some(ParseableStr {
            position,
            s: &s[0..pos]
        })
    }

    /// Split at the first occurrence of `needle`, returning the parts
    /// before and after it.
    fn split_at_str(self, needle: &str) -> Option<(ParseableStr<'t>, ParseableStr<'t>)> {
        let ParseableStr { position, s } = self;
        let pos = s.find(needle)?;
        Some((
            ParseableStr {
                position,
                s: &s[0..pos]
            },
            ParseableStr {
                position: position + pos + needle.len(),
                s: &s[pos + needle.len()..]
            }
        ))
    }

    /// Take every character for which `pred` returns true, return the
    /// string making up those characters and the remainder.
    fn take_while(self, mut pred: impl FnMut(char) -> bool)
                  -> (ParseableStr<'t>, ParseableStr<'t>) {
        let ParseableStr { position, s } = self;
        for (pos, c) in s.char_indices() {
            if !pred(c) {
                return (
                    ParseableStr {
                        position,
                        s: &s[0..pos]
                    },
                    ParseableStr {
                        position: position + pos,
                        s: &s[pos..]
                    },
                );
            }
        }
        (
            self,
            ParseableStr { s: "", position: position + s.len() },
        )
    }

    /// Expect 1 character for which `pred` must return true, return
    /// the string making up the remainder.
    fn expect1_matching<'d>(self, pred: impl FnOnce(char) -> bool, desc: &'d str)
                        -> Result<ParseableStr<'t>, Box<Expected<'d>>> {
        let ParseableStr { position, s } = self;
        let err = || Err(Expected { desc, position }.into());
        if s.is_empty() {
            return err();
        }
        let mut cs = s.char_indices();
        let (_, c) = cs.next().unwrap();
        if ! pred(c) {
            return err();
        }
        let (pos, _) = cs.next().unwrap_or_else(|| (s.len(), ' '));
        Ok(ParseableStr { position: position + pos, s: &s[pos..] })
    }

    fn take_identifier(self)
                       -> Result<(ParseableStr<'t>, ParseableStr<'t>), Box<Expected<'static>>> {
        let rest = self.expect1_matching(
            |c| c.is_ascii_lowercase() && (c.is_ascii_alphabetic() || c == '_'),
            "[a-z_] followed by [a-z0-9_]*"
        )?;
        let rest = rest.drop_while(
            |c| c.is_ascii_lowercase() && (c.is_ascii_alphanumeric() || c == '_'));
        let ParseableStr { position, s } = self;
        Ok((
            ParseableStr { position, s: &s[0..(rest.position - self.position)] },
            rest
        ))
    }

    fn drop_while(self, mut pred: impl FnMut(char) -> bool) -> ParseableStr<'t> {
        let ParseableStr { position, s } = self;
        for (pos, c) in s.char_indices() {
            if !pred(c) {
                return ParseableStr {
                    position: position + pos,
                    s: &s[pos..]
                };
            }
        }
        ParseableStr {
            position: position + s.len(),
            s: ""
        }
    }

    fn take_n_while<'d>(self, n: usize, mut pred: impl FnMut(char) -> bool, desc: &'d str)
                    -> Result<(ParseableStr<'t>, ParseableStr<'t>), Box<Expected<'d>>> {
        let ParseableStr { position, s } = self;
        let mut cs = s.char_indices();
        for _i in 0..n {
            if let Some((pos, c)) = cs.next() {
                if !pred(c) {
                    // Should it report the begin of the n characters
                    // or the character failing? Or use Cow and
                    // generate a new message saying where the begin
                    // is? Or use another Expected type that carries
                    // the info, rather.
                    return Err(Expected {
                        desc,
                        position: position + pos,
                    }.into())
                }
            } else {
                // Eof
                return Err(Expected {
                    desc,
                    position: position + s.len(),
                }.into())
            }
        }
        let (pos, _) = cs.next().unwrap_or_else(|| (s.len(), ' '));
        Ok((
            ParseableStr {
                position: position,
                s: &s[0..pos]
            },
            ParseableStr {
                position: position + pos,
                s: &s[pos..]
            },
        ))
    }

    fn drop_whitespace(self) -> ParseableStr<'t> {
        self.drop_while(|c| c.is_whitespace())
    }

    /// Split on a fixed string, `separator`; the `separator` string
    /// is not included in the returned parts. If
    /// `omit_empty_last_item` is true, if there's an empty string
    /// after the last separator, it is not reported as an item.
    fn split_str<'n>(self, separator: &'n str, omit_empty_last_item: bool)
                     -> Box<dyn Iterator<Item = ParseableStr> + 'n>
        where 't: 'n
    {
        Box::new(Gen::new(|co| async move {
            let mut rest = self;
            loop {
                if omit_empty_last_item && rest.is_empty() {
                    break;
                }
                if let Some(p2) = rest.find_str(separator) {
                    co.yield_(rest.up_to(p2)).await;
                    rest = p2.skip_bytes(separator.len());
                } else {
                    co.yield_(rest).await;
                    break;
                }
            }
        }).into_iter())
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

    #[test]
    fn t_split_at_str() {
        assert_eq!("foo -- bar".into_parseable().split_at_str("---"),
                   None);
        assert_eq!("foo -- bar".into_parseable().split_at_str("--"),
                   Some((
                       ParseableStr::new("foo "),
                       ParseableStr { s: " bar", position: 6 },
                   )));
    }

    #[test]
    fn t_split_str() {
        let new = |position, s| ParseableStr { position, s };
        {
            let s = ParseableStr::new("foo - bar- baz -  bam");
            let r1: Vec<ParseableStr> = s.split_str(" - ", false).collect();
            assert_eq!(&*r1, &[
                new(0, "foo"),
                new(6, "bar- baz"),
                new(17, " bam")
            ]);
        }
        {
            let s = ParseableStr::new("foo - bar- baz - ");
            let r1: Vec<ParseableStr> = s.split_str(" - ", false).collect();
            assert_eq!(&*r1, &[
                new(0, "foo"),
                new(6, "bar- baz"),
                new(17, "")
            ]);
        }
        {
            let s = ParseableStr::new("foo - bar- baz - ");
            let r1: Vec<ParseableStr> = s.split_str(" - ", true).collect();
            assert_eq!(&*r1, &[
                new(0, "foo"),
                new(6, "bar- baz"),
            ]);
        }
        {
            let s = ParseableStr::new(" - ");
            let r1: Vec<ParseableStr> = s.split_str(" - ", false).collect();
            assert_eq!(&*r1, &[
                new(0, ""),
                new(3, ""),
            ]);
        }
        {
            let s = ParseableStr::new(" - ");
            let r1: Vec<ParseableStr> = s.split_str(" - ", true).collect();
            assert_eq!(&*r1, &[
                new(0, ""),
            ]);
        }
        {
            let s = ParseableStr::new("");
            let r1: Vec<ParseableStr> = s.split_str(" - ", false).collect();
            assert_eq!(&*r1, &[
                new(0, ""),
            ]);
        }
        {
            let s = ParseableStr::new("");
            let r1: Vec<ParseableStr> = s.split_str(" - ", true).collect();
            assert_eq!(&*r1, &[
            ]);
        }
    }
    
    #[test]
    fn t_take_while() {
        assert_eq!("".into_parseable().take_while(|c| c.is_alphanumeric()),
                   (
                       ParseableStr::new(""),
                       ParseableStr::new("")
                   ));
        assert_eq!(" abc def".into_parseable().take_while(|c| c.is_alphanumeric()),
                   (
                       "".into_parseable(),
                       " abc def".into_parseable(),
                   ));
        assert_eq!("abc def".into_parseable().take_while(|c| c.is_alphanumeric()),
                   (
                       ParseableStr::new("abc"),
                       ParseableStr {
                           s: " def",
                           position: 3
                       }
                   ));
        assert_eq!("abcdef".into_parseable().take_while(|c| c.is_alphanumeric()),
                   (
                       ParseableStr::new("abcdef"),
                       ParseableStr {
                           s: "",
                           position: 6
                       }
                   ));
    }

    #[test]
    fn t_take_n_while() {
        let t = |s, n| ParseableStr::new(s).take_n_while(
            n, |c| c.is_ascii_digit(), "digit as part of year number");
        let ok = |s0, position, s1| Ok((ParseableStr::new(s0), ParseableStr {
            position,
            s: s1
        }));
        let err = |position, desc| Err(Box::new(Expected { position, desc }));
        assert_eq!(t("2024-10", 4), ok("2024", 4, "-10"));
        assert_eq!(t("2024-10", 3), ok("202", 3, "4-10"));
        assert_eq!(t("2024-10", 0), ok("", 0, "2024-10"));
        assert_eq!(t("2024-10", 5), err(4, "digit as part of year number"));
        assert_eq!(t("2024", 5), err(4, "digit as part of year number"));
        assert_eq!(t("2024", 4), ok("2024", 4, ""));
    }
}
