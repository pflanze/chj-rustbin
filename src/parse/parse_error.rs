

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub position: usize,
    pub location: Vec<FileLocation>
}

impl ParseError {
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

    pub fn message_append(mut self, s: &str) -> Self {
        self.message.push_str(s);
        self
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
