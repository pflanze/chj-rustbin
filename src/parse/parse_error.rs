use std::rc::Rc;
use std::{fmt::Write, sync::Arc};

// Can't do this with ToOwned, because ToOwned is auto-implemented
// when Clone exists, and ParseableStr needs Clone, thus ParseableStr
// cannot have a custom ToOwned, and any struct containing
// ParseableStr and has Clone can't either. Not exactly clear about
// the reasoning of this default impl here. So, make a new trait.

// Could/should I specify that Self and Owning are the same type with
// just a different type parameter?
pub trait IntoOwningBacking<S: AsRef<str>> {
    type Owning;
    fn into_owning_backing(self) -> Self::Owning;
}

/// A Source for parsing (XX rename it to Source?), without the
/// location info.
pub trait Backing: AsRef<str> + ToOwned + PartialEq {}

impl Backing for str {}
impl<'t> Backing for &'t str {}
impl Backing for String {}
impl<'t> Backing for &'t String {}
impl Backing for Arc<str> {}
impl Backing for Rc<str> {}

// Sigh, can't for these, 2 step, and I can't implement intermediary
// either. Would have to newtype. Wait, but newtype would impl
// AsRef<str>, but String does that, too, no? So what's up?
// impl Backing for Arc<String> {}
// impl Backing for Rc<String> {}

pub trait ParseContext: PartialEq {
    /// Shows the error with the context in the original string.
    /// `original_input` must be the original string on which this
    /// error is based (so that position matches up).
    fn show_context(&self, out: &mut String);
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct NoContext;

impl ParseContext for NoContext {
    fn show_context(&self, out: &mut String) {
        // still a hack?
        out.push_str("(no context)")
    }
}

/// A context for a parse error: the document ("backing") and position
/// in it. Unlike ParseableStr, owns the Backing, and does not contain
/// the parsing window.
#[derive(Debug)]
pub struct StringParseContext<B: Backing> {
    pub position: usize,
    pub backing: B,
}

// Conflicting implementation in core--aha?: C includes itself.
// impl<C: ParseContext, B: Backing> From<C> for StringParseContext<B> {
//     fn from(value: C) -> Self {
//     }
// }

impl<'t, B: Backing> IntoOwningBacking<B::Owned> for StringParseContext<&'t B>
where
    B::Owned: Backing,
    &'t B: Backing,
{
    type Owning = StringParseContext<B::Owned>;

    fn into_owning_backing(self) -> Self::Owning {
        let Self { position, backing } = self;
        StringParseContext {
            position,
            backing: backing.to_owned(),
        }
    }
}

// Do not use derive because then StringParseContext would *always*
// require S: Clone.
impl<B: Backing + Clone> Clone for StringParseContext<B> {
    fn clone(&self) -> Self {
        Self {
            position: self.position,
            backing: self.backing.clone(),
        }
    }
}

impl<B: Backing> PartialEq for StringParseContext<B> {
    fn eq(&self, other: &Self) -> bool {
        self.position == other.position
            && self.backing.as_ref() == other.backing.as_ref()
    }
}

impl<B: Backing> ParseContext for StringParseContext<B> {
    fn show_context(&self, out: &mut String) {
        let remainder = &self.backing.as_ref()[self.position..];
        if remainder.is_empty() {
            out.push_str(" at end of input")
        } else {
            write!(out, " at {:?}", remainder).expect("no err on string")
        }
    }
}

#[derive(Debug)]
pub struct ParseError<C: ParseContext> {
    pub message: String,
    pub context: C,
    pub backtrace: Vec<FileLocation>,
}

impl<'t, B: Backing> IntoOwningBacking<B::Owned>
    for ParseError<StringParseContext<&'t B>>
where
    B::Owned: Backing,
    &'t B: Backing,
{
    type Owning = ParseError<StringParseContext<B::Owned>>;

    fn into_owning_backing(self) -> Self::Owning {
        let ParseError {
            message,
            context,
            backtrace,
        } = self;
        ParseError {
            message,
            context: context.into_owning_backing(),
            backtrace,
        }
    }
}

impl<C: ParseContext> ParseError<C> {
    pub fn to_string_showing_location(&self, show_backtrace: bool) -> String {
        let ParseError {
            message, context, ..
        } = self;
        let mut message = message.clone();
        context.show_context(&mut message);
        if show_backtrace {
            self.show_backtrace(&mut message);
        }
        message
    }

    /// 'Backtrace' with the leaf location at the top, one location
    /// per line, indented by a tab, starting with a newline.
    pub fn show_backtrace(&self, out: &mut String) {
        for l in &self.backtrace {
            out.push_str("\n\t");
            out.push_str(&l.to_string());
        }
    }

    /// Append `s` to the message in self (functionally).
    pub fn message_append(mut self, s: &str) -> Self {
        self.message.push_str(s);
        self
    }
}

/// Only the message and context are compared (this is to allow for
/// tests without breaking due to backtrace changes; backtrace should be
/// just debugging information)
impl<C: ParseContext> PartialEq for ParseError<C> {
    fn eq(&self, other: &Self) -> bool {
        self.message == other.message && self.context == other.context
    }
}

#[derive(Debug)]
pub struct FileLocation {
    pub file: &'static str,
    pub line: u32,
    pub column: u32,
}

impl FileLocation {
    pub fn to_string(&self) -> String {
        format!("{}:{}:{}", self.file, self.line, self.column)
    }
}

#[macro_export]
macro_rules! file_location {
    {} => {
        $crate::parse::parse_error::FileLocation {
            file: file!(),
            line: line!(),
            column: column!(),
        }
    }
}

#[macro_export]
macro_rules! parse_error {
    { $($body:tt)* } => {
        ParseError {
            backtrace: vec![$crate::file_location!()],
            $($body)*
        }
    }
}

/// Define the type `LocalParseErrorContext` to a type that implements
/// `ParseContext` where using this macro!
#[macro_export]
macro_rules! T {
    { $e:expr } => {
        $e.map_err(|e| {
            let mut e: $crate::parse::parse_error::ParseError<_> = e.into();
            e.backtrace.push($crate::file_location!());
            e
        })
    }
}
