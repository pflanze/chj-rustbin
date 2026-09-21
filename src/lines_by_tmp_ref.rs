use std::{
    fs::File,
    io::{BufRead, BufReader},
    sync::Arc,
};

use crate::by_tmp_ref_iterator::ByTmpRefIterator;

pub fn trim_line_terminator(line: &[u8], line_terminator: u8) -> &[u8] {
    if line.last().copied() == Some(line_terminator) {
        &line[0..line.len() - 1]
    } else {
        &line
    }
}

pub struct ReadLinesByTmpRef {
    input: BufReader<File>,
    line_buf: Vec<u8>,
    line_terminator: u8,
}

impl ReadLinesByTmpRef {
    pub fn new(input: BufReader<File>, line_terminator: u8) -> Self {
        Self {
            input,
            line_buf: Vec::new(),
            line_terminator,
        }
    }
}

impl ByTmpRefIterator for ReadLinesByTmpRef {
    type Item = [u8];
    type Error = std::io::Error;

    fn next_tmp_ref<'s>(&'s mut self) -> Result<Option<&'s [u8]>, Self::Error> {
        let Self {
            input,
            line_buf,
            line_terminator,
        } = self;
        line_buf.clear();
        let n = input.read_until(*line_terminator, line_buf)?;
        if n == 0 {
            Ok(None)
        } else {
            Ok(Some(&*line_buf))
        }
    }
}

pub struct ContentLinesByTmpRef {
    content: Arc<[u8]>,
    line_terminator: u8,
    position: usize,
}

impl ContentLinesByTmpRef {
    pub fn new(content: Arc<[u8]>, line_terminator: u8) -> Self {
        Self {
            content,
            line_terminator,
            position: 0,
        }
    }
}

impl ByTmpRefIterator for ContentLinesByTmpRef {
    type Item = [u8];
    type Error = std::io::Error;

    fn next_tmp_ref<'s>(&'s mut self) -> Result<Option<&'s [u8]>, Self::Error> {
        let Self {
            content,
            line_terminator,
            position,
        } = self;
        let c = &**content;
        if *position < c.len() {
            let rest = &c[*position..];
            let line = if let Some(pos) =
                rest.iter().position(|b| b == line_terminator)
            {
                let pos_after = pos + 1;
                *position += pos_after;
                &rest[..pos_after]
            } else {
                *position = c.len();
                rest
            };
            Ok(Some(line))
        } else {
            Ok(None)
        }
    }
}
