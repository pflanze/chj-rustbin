

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub position: usize,
    pub location: Vec<FileLocation>
}

impl ParseError {
    /// Shows the error with the context in the original string.
    /// `original_input` must be the original string on which this
    /// error is based (so that position matches up). Does *not*
    /// include the backtrace.
    pub fn to_string_in_context(&self, original_input: &str) -> String {
        let ParseError { message, position, .. } = self;
        let remainder = &original_input[*position..];
        if remainder.is_empty() {
            format!("{message} at end of input")
        } else {
            format!("{message} at {:?}", remainder)
        }
    }

    /// 'Backtrace' with the leaf location at the top, one location
    /// per line, indented by a tab.
    pub fn backtrace(&self) -> String {
        let mut out = String::new();
        for l in &self.location {
            out.push_str("\n\t");
            out.push_str(&l.to_string());
        }
        out
    }

    /// Append `s` to the message in self (functionally).
    pub fn message_append(mut self, s: &str) -> Self {
        self.message.push_str(s);
        self
    }
}

/// Only the message and position are compared (this is to allow for
/// tests without breaking due to location changes; location should be
/// just debugging information)
impl PartialEq for ParseError {
    fn eq(&self, other: &Self) -> bool {
        self.message == other.message
            && self.position == other.position
    }
}

#[derive(Debug)]
pub struct FileLocation {
    pub file: &'static str,
    pub line: u32,
    pub column: u32
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
            location: vec![$crate::file_location!()],
            $($body)*
        }
    }
}

#[macro_export]
macro_rules! T {
    { $e:expr } => {
        $e.map_err(|e| {
            let mut e: $crate::parse::parse_error::ParseError = e.into();
            e.location.push($crate::file_location!());
            e
        })
    }
}
