use ref_cons_list::List;

use crate::syntax::{
    standard_skip_string_like, ContentWithPosition, IsWordChar, NestContext,
    SubKind, Syntax, SyntaxError, SyntaxKind, SUB_KIND_NONE,
};

pub struct Rust;

pub const SUB_KIND_BYTE: SubKind = SubKind(1);
pub const SUB_KIND_UNICODE: SubKind = SubKind(2);

pub const SUB_KIND_COLON: SubKind = SubKind(3);
pub const SUB_KIND_VERTICAL_BAR: SubKind = SubKind(3);

pub const SUB_KIND_OR: SubKind = SubKind(9);
pub const SUB_KIND_AND: SubKind = SubKind(10);

pub const SUB_KIND_ADD: SubKind = SubKind(11);
pub const SUB_KIND_SUB: SubKind = SubKind(12);
pub const SUB_KIND_MUL: SubKind = SubKind(13);
pub const SUB_KIND_DIV: SubKind = SubKind(14);
pub const SUB_KIND_REM: SubKind = SubKind(15);

pub const SUB_KIND_EQUAL: SubKind = SubKind(20);
// `->`
pub const SUB_KIND_ARROW: SubKind = SubKind(21);

// A tick, in `< >` context.
pub const SUB_KIND_LIFETIME: SubKind = SubKind(30);

impl Syntax for Rust {
    fn is_word_char(&self, c: super::bchar) -> IsWordChar {
        if c.is_ascii_alphabetic() || c.is_ascii_digit() || c == b'_' {
            IsWordChar::NonEnding
        } else if c == b'!' {
            IsWordChar::Ending
        } else {
            IsWordChar::No
        }
    }

    fn is_number_char(&self, c: super::bchar) -> IsWordChar {
        if c.is_ascii_digit()
            || match c {
                b'.' | b'f' | b'e' => true,
                // type suffixes
                b'u' => true,
                _ => false,
            }
        {
            IsWordChar::NonEnding
        } else {
            IsWordChar::No
        }
    }

    fn syntax_kind_at<'c>(
        &self,
        content_with_position: ContentWithPosition<'c>,
        nest_context: &List<NestContext>,
    ) -> Result<(SyntaxKind, ContentWithPosition<'c>), SyntaxError> {
        let c = content_with_position[0];
        use SyntaxKind::*;
        let (kind, offset) = match c {
            b'(' => (
                Open {
                    opening: c,
                    expected_closing: b')',
                },
                0,
            ),
            b'{' => (
                Open {
                    opening: c,
                    expected_closing: b'}',
                },
                0,
            ),
            b'[' => (
                Open {
                    opening: c,
                    expected_closing: b']',
                },
                0,
            ),
            b'<' => (
                Open {
                    opening: c,
                    expected_closing: b'>',
                },
                0,
            ),

            b')' => (
                Close {
                    expected_opening: b'(',
                    closing: c,
                },
                0,
            ),
            b'}' => (
                Close {
                    expected_opening: b'{',
                    closing: c,
                },
                0,
            ),
            b']' => (
                Close {
                    expected_opening: b'[',
                    closing: c,
                },
                0,
            ),
            b'>' => (
                Close {
                    expected_opening: b'<',
                    closing: c,
                },
                0,
            ),

            b'"' => (StringLiteral(SUB_KIND_UNICODE), 0),

            b'\'' => {
                // In type context, a tick is a lifetime, otherwise a
                // char literal
                if nest_context.any(|nest_context| {
                    // println!("nest_context = {nest_context}");
                    let b = match nest_context.syntax_kind {
                        Open { opening, expected_closing: _ } => opening == b'<',
                        _ => unreachable!("currently nest context is only built for opening contexts")
                    };
                    // dbg!(b);
                    b
                }) {
                    (Other(SUB_KIND_LIFETIME), 0)
                } else {
                    (CharLiteral(SUB_KIND_UNICODE), 0)
                }
            }

            b':' => (Other(SUB_KIND_COLON), 0),
            b';' => (Semicolon(SUB_KIND_NONE), 0),

            b'b' => match content_with_position.get(1) {
                Some(b'\"') => (StringLiteral(SUB_KIND_BYTE), 1),
                Some(b'\'') => (CharLiteral(SUB_KIND_BYTE), 1),
                _ => (Word(SUB_KIND_NONE), 0),
            },
            b'|' => match content_with_position.get(1) {
                // XX or a thunk!
                Some(b'|') => (Operator(SUB_KIND_OR), 1),
                // XX pattern match alternative, closure, binary or, ?
                _ => (Other(SUB_KIND_VERTICAL_BAR), 0),
            },
            b'=' => match content_with_position.get(1) {
                Some(b'=') => (Operator(SUB_KIND_EQUAL), 1),
                _ => (Assignment(SUB_KIND_NONE), 0),
            },
            b'+' => match content_with_position.get(1) {
                Some(b'=') => (Assignment(SUB_KIND_ADD), 1),
                _ => (Operator(SUB_KIND_ADD), 0),
            },
            b'-' => match content_with_position.get(1) {
                Some(b'>') => (Other(SUB_KIND_ARROW), 1),
                _ => (Operator(SUB_KIND_SUB), 0),
            },
            b'/' => match content_with_position.get(1) {
                Some(b'/') =>
                // `//` comment
                {
                    (LineComment(SUB_KIND_NONE), 1)
                }
                Some(b'*') =>
                // `/* .. */` comment
                {
                    (DelimitedComment(SUB_KIND_NONE), 1)
                }
                Some(_) | None => (Operator(SUB_KIND_DIV), 0),
            },

            // XX various other things, just fall back to generic?
            c if c.is_ascii_alphabetic() => (Word(SUB_KIND_NONE), 0),
            c if c.is_ascii_digit() => (NumberLiteral(SUB_KIND_NONE), 0),

            _ => (Other(SUB_KIND_NONE), 0),
        };
        // eprintln!("{kind}");
        Ok((kind, content_with_position.advance(1 + offset)))
    }

    fn skip_delimited_comment<'c>(
        &self,
        content_with_position: ContentWithPosition<'c>,
    ) -> Result<ContentWithPosition<'c>, SyntaxError> {
        let ContentWithPosition {
            content,
            mut position,
        } = content_with_position;
        while position < content.len() {
            match content[position] {
                b'*' => match content.get(position + 1) {
                    Some(b'/') => {
                        return Ok(ContentWithPosition {
                            content,
                            position: position + 2,
                        })
                    }
                    _ => (),
                },
                _ => (),
            }
            position += 1;
        }
        Err(SyntaxError::Missing("*/"))
    }

    fn skip_string_literal<'c>(
        &self,
        content_with_position: ContentWithPosition<'c>,
        _sub_kind: SubKind,
    ) -> Result<ContentWithPosition<'c>, SyntaxError> {
        // sub_kind (byte or normal) does not matter, only difference
        // for skipping is the start that was already skipped.
        standard_skip_string_like(content_with_position, b'"')
    }

    fn skip_char_literal<'c>(
        &self,
        content_with_position: ContentWithPosition<'c>,
        _sub_kind: SubKind,
    ) -> Result<ContentWithPosition<'c>, SyntaxError> {
        // sub_kind (byte or normal) does not matter, only difference
        // for skipping is the start that was already skipped.
        standard_skip_string_like(content_with_position, b'\'')
    }
}

#[cfg(test)]
mod tests {
    use bstr::{BStr, ByteSlice};
    use ref_cons_list::List;

    use super::*;

    // use crate::bstr;

    fn b<'s>(s: &'s str) -> &'s BStr {
        s.as_ref()
    }

    fn c<'s>(s: &'s str) -> ContentWithPosition<'s> {
        ContentWithPosition {
            content: b(s),
            position: 0,
        }
    }

    fn skip1<'s>(content: &'s str) -> Result<Option<&'s str>, SyntaxError> {
        let c0 = c(content);
        match Rust.skip_one(c0, &List::Null) {
            Ok(None) => Ok(None),
            Ok(Some(c1)) => {
                // dbg!(c1.as_bstr());
                // dbg!(c0.up_to(c1));
                Ok(Some(c1))
            }
            Err(SyntaxError::UnbalancedNesting {
                position,
                expected,
                found,
            }) => {
                // eprintln!(
                //     "expected {:?}, found {:?} in region: {:?}",
                //     bstr!(expected),
                //     bstr!(found),
                //     &content[..position]
                // );
                Err(SyntaxError::UnbalancedNesting {
                    position,
                    expected,
                    found,
                })
            }
            Err(e) => Err(e),
        }
        .map(|opt| opt.map(|s| s.as_bstr().to_str().unwrap()))
    }

    #[test]
    fn t0() {
        let t = |s| skip1(s).unwrap();
        let te = |s| skip1(s).err().unwrap().to_string();
        assert_eq!(t("foo").as_deref(), Some(""));
        assert_eq!(t("foo bar").as_deref(), Some(" bar"));
        assert_eq!(t("<>").as_deref(), Some(""));
        assert_eq!(t("<a,b>").as_deref(), Some(""));
        assert_eq!(t("{a,b}").as_deref(), Some(""));
        assert_eq!(t("{<a,b>}").as_deref(), Some(""));
        assert_eq!(t("<{a,b}>hi").as_deref(), Some("hi"));

        assert_eq!(t("<const {a,b}>hi").as_deref(), Some("hi"));
        assert_eq!(t("<const BAR: {a,b}>hi").as_deref(), Some("hi"));
        assert_eq!(t("<const BAR: usize {a,b}>hi").as_deref(), Some("hi"));
        assert_eq!(t("<const BAR: usize = {a,b}>hi").as_deref(), Some("hi"));

        assert_eq!(t("{a,b} x ").as_deref(), Some(" x "));

        assert_eq!(t("{a+b} x ").as_deref(), Some(" x "));

        assert_eq!(t("{a,b } x ").as_deref(), Some(" x "));

        assert_eq!(t("{a+b } x ").as_deref(), Some(" x "));

        assert_eq!(
            te("<const BAR: usize = {a+b } x "),
            "missing character \">\""
        );

        assert_eq!(
            t("<const BAR: usize = {a+b } x >hi").as_deref(),
            Some("hi")
        );

        assert_eq!(t("<const BAR: usize = {a+b}>hi").as_deref(), Some("hi"));
        assert_eq!(t("<const BAR: usize = {a + b}>hi").as_deref(), Some("hi"));
        assert_eq!(t("<const BAR: usize = {1 + 3}>hi").as_deref(), Some("hi"));

        assert_eq!(
            t("<const BAR: usize = { 1 + 3 }> Foo").as_deref(),
            Some(" Foo")
        );
    }

    #[test]
    fn t1() -> Result<(), SyntaxError> {
        let content = b"impl<const BAR: usize = { 1 + 3 }> Foo".as_bstr();
        let r = Rust;
        {
            let mut p = ContentWithPosition {
                content,
                position: 0,
            };
            let mut kind;
            (kind, p) = r.syntax_kind_at(p, &List::Null)?;
            assert_eq!(kind, SyntaxKind::Word(SUB_KIND_NONE));
            assert_eq!(p.position, 1);
            (kind, p) = r.syntax_kind_at(p.advance(3), &List::Null)?;
            assert_eq!(
                kind,
                SyntaxKind::Open {
                    opening: b'<',
                    expected_closing: b'>'
                }
            );
            assert_eq!(p.position, 5);
        }
        {
            let mut p = ContentWithPosition {
                content,
                position: 0,
            };
            let mut p1;

            p1 = r.skip_one(p, &List::Null)?.unwrap();
            assert_eq!(p1.position, 4);
            assert_eq!(p.up_to(p1), b("impl"));
            p = p1;

            p1 = r.skip_one(p, &List::Null)?.unwrap();
            assert_eq!(p1.position, 34);
            assert_eq!(p.up_to(p1), b("<const BAR: usize = { 1 + 3 }>"));
            // p = p1;
        }
        Ok(())
    }

    fn tpos<'s>(
        content: &'s str,
        position: usize,
        until: &str,
    ) -> Result<Option<&'s BStr>, String> {
        Rust.skip_until_in_level(
            ContentWithPosition {
                content: content.as_ref(),
                position,
            },
            |cwp| {
                let b = cwp.starts_with(until.as_ref());
                // eprintln!("cwp {:?} starts with {until:?}: {b}", cwp.as_bstr());
                b
            },
        )
        .map(|opt| opt.map(|cwp| cwp.as_bstr()))
        .map_err(|e| e.to_string())
    }

    #[test]
    fn t2() {
        let content = "impl<FOO, const BAR: usize = { 1 + 3 }> Foo<Foo<BAZ>> // } note: not { here \n\
                       for Bar /* not } { here either */ \n\
                       where // } { \n\
                       { fn some";
        assert_eq!(tpos(content, 0, "{").ok(), Some(Some(b("{ fn some"))));
        assert_eq!(
            tpos(content, 0, "3").err().as_deref(),
            Some("missing character \"}\"")
        );
    }

    #[test]
    fn t_lifetimes() {
        let content = r#"pub fn standard_skip_string_like<'c>(
    mut content_with_position: ContentWithPosition<'c>,
    quote_char: bchar,
) -> Result<ContentWithPosition<'c>, SyntaxError> {ok"#;
        assert_eq!(tpos(content, 0, " {").unwrap(), Some(b(" {ok")));

        let content = r#"pub fn standard_skip_string_like<'c>(
    mut content_with_position: ContentWithPosition<'c>,
    quote_char: bchar,
) -> Result<ContentWithPosition<'c>, SyntaxError>
     'c' b'c' 'b'
{ok"#;
        assert_eq!(tpos(content, 0, "\n{").unwrap(), Some(b("\n{ok")));
    }
}
