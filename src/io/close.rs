//! Convenience around calling `flush` and dropping the file handle

use std::{fs::File, io::Write};

pub trait Close {
    fn close(self) -> std::io::Result<()>;
}

impl Close for File {
    fn close(mut self) -> std::io::Result<()> {
        self.flush()
    }
}
