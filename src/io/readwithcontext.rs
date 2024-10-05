//! File handling utilities that make life simpler for the common
//! case.

use std::{path::Path, fs::File, io::{BufReader, BufRead}};
use anyhow::{anyhow, Result, Context};


pub fn trim(line: &mut String) {
    if line.ends_with("\n") {
        line.pop().unwrap();
    }
}

/// Easy buffered file opening with context on open errors. But see
/// `ReadWithContext` instead for keeping context across subsequent
/// calls, too.
pub fn open_file(path: &Path) -> Result<BufReader<File>> {
    Ok(BufReader::new(File::open(path).with_context(
        || format!("opening file {:?}", path))?))
}

/// "Clean" read_line function: returns true if it did read a line,
/// false on EOF. Does overwrite `line`, not append to it. Removes
/// trailing '\n' if present.
pub fn easy_read_line(inp: &mut BufReader<File>, line: &mut String) -> Result<bool> {
    line.clear();
    if inp.read_line(line)? != 0 {
        trim(line);
        Ok(true)
    } else {
        Ok(false)
    }
}

/// Automatically count lines and report them and the path in error
/// messages
pub struct ReadWithContext<'p> {
    path: &'p Path,
    linenumber: i64,
    reader: BufReader<File>,
}

impl<'p> ReadWithContext<'p> {
    pub fn open_path(path: &'p Path) -> Result<ReadWithContext<'p>> {
        Ok(ReadWithContext {
            path,
            linenumber: 0,
            reader: open_file(path)?
        })
    }
    /// "Clean" read_line function: returns true if it did read a line,
    /// false on EOF. Does overwrite `line`, not append to it. Removes
    /// trailing '\n' if present.
    pub fn easy_read_line(&mut self, line: &mut String) -> Result<bool> {
        self.linenumber += 1;
        easy_read_line(&mut self.reader, line).with_context(
            || anyhow!("file {:?} line {}", self.path, self.linenumber))
    }


    /// Report an error in the context of this file and position
    #[allow(unused)]
    pub fn err_with_context<T>(&self, err: anyhow::Error) -> Result<T, anyhow::Error>
    {
        Err(err.context(anyhow!("file {:?} line {}", self.path, self.linenumber)))
    }

    /// A Result in the context of this file and position
    #[allow(unused)]
    pub fn context<T>(&self, res: Result<T, anyhow::Error>) -> Result<T, anyhow::Error>
    {
        match res {
            Ok(v) => Ok(v),
            Err(e) => self.err_with_context(e)
        }
    }

}

