pub mod rust;

use std::{
    fmt::{Debug, Display},
    ops::{Deref, Index},
};

use bstr::BStr;
use ref_cons_list::{cons, List};

use crate::bstr;

pub fn standard_skip_string_like<'c>(
    mut content_with_position: ContentWithPosition<'c>,
    quote_char: bchar,
) -> Result<ContentWithPosition<'c>, SyntaxError> {
    while !content_with_position.is_empty() {
        let c = content_with_position[0];
        if c == quote_char {
            return Ok(content_with_position.advance(1));
        }
        if c == b'\\' {
            // XXX do not overflow strings, please!
            content_with_position.position += 2;
        } else {
            content_with_position.position += 1;
        }
        // Unicode escapes should not matter for skipping?
    }
    // dbg!(quote_char);
    Err(SyntaxError::MissingChar(quote_char))
}

macro_rules! return_on_none {
    { $e:expr } => {
        match $e {
            Some(v) => v,
            None => return Ok(None),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ContentWithPosition<'content> {
    pub content: &'content BStr,
    pub position: usize,
}

impl<'content> Deref for ContentWithPosition<'content> {
    type Target = BStr;

    fn deref(&self) -> &Self::Target {
        let Self { content, position } = self;
        &content[*position..]
    }
}

impl<'content> Index<usize> for ContentWithPosition<'content> {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        let Self { content, position } = self;
        &content[*position + index]
    }
}

impl<'content> ContentWithPosition<'content> {
    pub fn as_bstr(&self) -> &'content BStr {
        let Self { content, position } = self;
        &content[*position..]
    }

    /// Must be run with the same content!
    pub fn up_to(&self, to: Self) -> &BStr {
        &self.content[self.position..to.position]
    }

    pub fn get(&self, offset: usize) -> Option<u8> {
        let Self { content, position } = self;
        content.get(*position + offset).copied()
    }

    pub fn advance(&self, offset: usize) -> Self {
        let Self { content, position } = self;
        Self {
            content,
            position: position + offset,
        }
    }
}

#[allow(non_camel_case_types)]
type bchar = u8;

/// Custom subkinds
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SubKind(pub u8);
pub const SUB_KIND_NONE: SubKind = SubKind(0);

/// What kind of syntax element is expected to come (before being
/// parsed, i.e. there can be an error)
///
/// Note that the `Syntax` trait expects simple syntax that has no
/// ambiguity.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SyntaxKind {
    LineComment(SubKind),
    DelimitedComment(SubKind),
    /// Can be a keyword, identifier, etc.
    Word(SubKind),
    Operator(SubKind),
    Assignment(SubKind),
    /// Or whatever is used for seqencing
    Semicolon(SubKind),
    Other(SubKind),
    NumberLiteral(SubKind),
    StringLiteral(SubKind),
    CharLiteral(SubKind),
    Open {
        opening: bchar,
        expected_closing: bchar,
    },
    Close {
        expected_opening: bchar,
        closing: bchar,
    },
    Unknown(bchar),
}

#[test]
fn t_size_syntax_kind() {
    assert_eq!(size_of::<SyntaxKind>(), 3);
}

impl Display for SyntaxKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SyntaxKind::Open {
                opening,
                expected_closing,
            } => write!(
                f,
                "Open{{ {:?}, {:?} }}",
                bstr!(*opening),
                bstr!(*expected_closing)
            ),
            SyntaxKind::Close {
                expected_opening,
                closing,
            } => write!(
                f,
                "Close{{ {:?}, {:?} }}",
                bstr!(*expected_opening),
                bstr!(*closing)
            ),
            _ => write!(f, "{self:?}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct NestContext {
    pub syntax_kind: SyntaxKind,
    pub start_position: usize,
}

impl Display for NestContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            syntax_kind,
            start_position,
        } = self;
        write!(f, "NestContext {{ syntax_kind: {syntax_kind}, start_position: {start_position} }}")
    }
}

#[derive(thiserror::Error)]
pub enum SyntaxError {
    #[error("missing {0:?}")]
    Missing(&'static str),
    #[error("missing character {:?}", bstr!(*.0))]
    MissingChar(bchar),
    #[error("unbalanced nesting, expected closing character {}, got {} at position {position}",
            bstr!(*expected),
            bstr!(*found),
    )]
    UnbalancedNesting {
        position: usize,
        expected: bchar,
        found: bchar,
    },
    #[error("unexpected closing character {} at position {position}", bstr!(*found))]
    UnexpectedClosing { position: usize, found: bchar },
    // #[error("unexpected end of input {0}")]
    // UnexpectedEof(&'static str),
}

impl Debug for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SyntaxError: {self}")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IsWordChar {
    No,
    /// A word char that does not imply ending the word
    NonEnding,
    /// A word char that implies ending the word
    Ending,
}

pub trait Syntax {
    /// Receives a slice starting with non-whitespace, the initial
    /// part of a piece of syntax. Returns the kind and the contents
    /// after the syntax initiator.
    fn syntax_kind_at<'c>(
        &self,
        content_with_position: ContentWithPosition<'c>,
        nest_context: &List<NestContext>,
    ) -> Result<(SyntaxKind, ContentWithPosition<'c>), SyntaxError>;

    /// The position after the syntax that ends the comment. For
    /// comments ended by a newline, simply use `skip_line` instead.
    ///
    /// `content_with_position` must point *after* the starting syntax of the comment
    fn skip_delimited_comment<'c>(
        &self,
        content_with_position: ContentWithPosition<'c>,
    ) -> Result<ContentWithPosition<'c>, SyntaxError>;

    fn skip_string_literal<'c>(
        &self,
        content_with_position: ContentWithPosition<'c>,
        sub_kind: SubKind,
    ) -> Result<ContentWithPosition<'c>, SyntaxError>;

    fn skip_char_literal<'c>(
        &self,
        content_with_position: ContentWithPosition<'c>,
        sub_kind: SubKind,
    ) -> Result<ContentWithPosition<'c>, SyntaxError>;

    fn is_word_char(&self, c: bchar) -> IsWordChar;

    fn is_number_char(&self, c: bchar) -> IsWordChar;

    /// Returns None if, after skipping whitespace, EOF is reached
    fn skip_whitespace<'c>(
        &self,
        content_with_position: ContentWithPosition<'c>,
    ) -> Option<ContentWithPosition<'c>> {
        let ContentWithPosition {
            content,
            mut position,
        } = content_with_position;
        while position < content.len() {
            if !content[position].is_ascii_whitespace() {
                return Some(ContentWithPosition { content, position });
            }
            position += 1;
        }
        None
    }

    /// Skip whitespace then get the syntax kind of the next syntax element
    ///
    /// Returns the position after skipping whitespace (i.e. place of
    /// the syntax start), the syntax kind and the remainder of the
    /// syntax after.  Returns None if, after skipping whitespace, EOF
    /// is reached.
    fn syntax_kind<'c>(
        &self,
        content_with_position: ContentWithPosition<'c>,
        nest_context: &List<NestContext>,
    ) -> Result<
        Option<(usize, (SyntaxKind, ContentWithPosition<'c>))>,
        SyntaxError,
    > {
        let content_with_position =
            return_on_none!(self.skip_whitespace(content_with_position));
        Ok(Some((
            content_with_position.position,
            self.syntax_kind_at(content_with_position, nest_context)?,
        )))
    }

    /// The position after the next newline, or EOF.
    fn skip_line<'c>(
        &self,
        content_with_position: ContentWithPosition<'c>,
    ) -> ContentWithPosition<'c> {
        let ContentWithPosition {
            content,
            mut position,
        } = content_with_position;
        while position < content.len() {
            if content[position] == b'\n' {
                return ContentWithPosition {
                    content,
                    position: position + 1,
                };
            }
            position += 1;
        }
        ContentWithPosition { content, position }
    }

    fn skip_word<'c>(
        &self,
        mut content_with_position: ContentWithPosition<'c>,
    ) -> ContentWithPosition<'c> {
        while !content_with_position.is_empty() {
            let c = content_with_position[0];
            match self.is_word_char(c) {
                IsWordChar::No => return content_with_position,
                IsWordChar::Ending => return content_with_position.advance(1),
                IsWordChar::NonEnding => (),
            }
            content_with_position.position += 1;
        }
        content_with_position
    }

    fn skip_number<'c>(
        &self,
        mut content_with_position: ContentWithPosition<'c>,
    ) -> ContentWithPosition<'c> {
        while !content_with_position.is_empty() {
            let c = content_with_position[0];
            match self.is_number_char(c) {
                IsWordChar::No => return content_with_position,
                IsWordChar::Ending => return content_with_position.advance(1),
                IsWordChar::NonEnding => (),
            }
            content_with_position.position += 1;
        }
        content_with_position
    }

    /// The position after skipping the first syntax item after
    /// skipping any whitespace
    ///
    /// `content_with_position` must point to the beginning of some
    /// syntax.
    fn skip_one<'c>(
        &self,
        content_with_position: ContentWithPosition<'c>,
        nest_context: &List<NestContext>,
    ) -> Result<Option<ContentWithPosition<'c>>, SyntaxError> {
        let (start_position, (kind, rest)) = return_on_none!(
            self.syntax_kind(content_with_position, nest_context)?
        );
        // Note: in the future, when not only doing skipping, the
        // "rest" might include the syntax start itself! Then this
        // will fail.
        assert!(rest.position > start_position);

        // eprintln!(
        //     "skip_one({:?}): kind={kind}, rest={:?}",
        //     content_with_position.as_bstr(),
        //     rest.as_bstr()
        // );

        #[allow(unused)]
        let content_with_position = ();
        match kind {
            SyntaxKind::LineComment(_sub_kind) => {
                Ok(Some(self.skip_line(rest)))
            }
            SyntaxKind::DelimitedComment(_sub_kind) => {
                Ok(Some(self.skip_delimited_comment(rest)?))
            }
            SyntaxKind::Word(_sub_kind) => Ok(Some(self.skip_word(rest))),
            SyntaxKind::Operator(_sub_kind) => Ok(Some(rest)),
            SyntaxKind::Assignment(_sub_kind) => Ok(Some(rest)),
            SyntaxKind::Semicolon(_sub_kind) => Ok(Some(rest)),
            SyntaxKind::Other(_sub_kind) => Ok(Some(rest)),
            SyntaxKind::NumberLiteral(_sub_kind) => {
                Ok(Some(self.skip_number(rest)))
            }
            SyntaxKind::StringLiteral(sub_kind) => {
                Ok(Some(self.skip_string_literal(rest, sub_kind)?))
            }
            SyntaxKind::CharLiteral(sub_kind) => {
                Ok(Some(self.skip_char_literal(rest, sub_kind)?))
            }
            SyntaxKind::Open {
                opening: _,
                expected_closing,
            } => {
                let mut p = rest;
                #[allow(unused)]
                let rest = ();

                while !p.is_empty() {
                    // eprintln!("going to skip_one, in {:?}", p.as_bstr());
                    match self.skip_one(
                        p,
                        &cons(
                            NestContext {
                                syntax_kind: kind.clone(),
                                start_position,
                            },
                            nest_context,
                        ),
                    ) {
                        Ok(Some(rest)) => p = rest,
                        Ok(None) => break,
                        Err(SyntaxError::UnexpectedClosing {
                            found,
                            position,
                        }) => {
                            if found == expected_closing {
                                p.position = position;
                                return Ok(Some(p));
                            } else {
                                return Err(SyntaxError::UnbalancedNesting {
                                    position: p.position,
                                    expected: expected_closing,
                                    found,
                                });
                            }
                        }
                        Err(e) => return Err(e),
                    }
                }
                // dbg!(expected_closing);
                Err(SyntaxError::MissingChar(expected_closing))
            }
            SyntaxKind::Close {
                expected_opening: _,
                closing,
            } => Err(SyntaxError::UnexpectedClosing {
                found: closing,
                position: rest.position,
            }),
            SyntaxKind::Unknown(_) => Ok(Some(rest)),
        }
    }

    /// Skip forward until `until` matches on the same level,
    /// i.e. while skipping over nested syntax.
    fn skip_until_in_level<'c>(
        &self,
        content_with_position: ContentWithPosition<'c>,
        until: impl Fn(ContentWithPosition) -> bool,
    ) -> Result<Option<ContentWithPosition<'c>>, SyntaxError> {
        let mut p =
            return_on_none!(self.skip_whitespace(content_with_position));
        while !p.is_empty() {
            if until(p) {
                return Ok(Some(p));
            }
            p = return_on_none!(self.skip_one(p, &List::Null)?);
        }
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use bstr::ByteSlice;

    use super::*;

    fn b<'s>(s: &'s str) -> &'s BStr {
        s.as_ref()
    }

    fn cwp<'s>(s: &'s str) -> ContentWithPosition<'s> {
        ContentWithPosition {
            content: b(s),
            position: 0,
        }
    }

    #[test]
    fn t_standard_skip_string_like() {
        fn t(s: &str, c: u8) -> Option<&str> {
            standard_skip_string_like(cwp(s), c)
                .ok()
                .map(|s| s.as_bstr().to_str().unwrap())
        }

        assert_eq!(t("foo a", b'"'), None);
        assert_eq!(t("foo\" a", b'"'), Some(" a"));
        assert_eq!(t("foo\" a", b'\''), None);
        assert_eq!(t("foo\' a", b'\''), Some(" a"));
    }
}
