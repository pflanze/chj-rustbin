use std::ops::Range;

use bstr::ByteSlice;
use rayon::iter::{
    IndexedParallelIterator, IntoParallelRefIterator, ParallelIterator,
};

use crate::util::io::file_as_mmap::FileAsMmap;

pub struct FileAsMmapWithLines {
    pub file_as_mmap: FileAsMmap,
    pub record_separator: u8,
    pub omit_empty_entry_after_last_record: bool,
    pub chunks: Vec<Range<usize>>,
}

impl FileAsMmapWithLines {
    /// `chunk_size` is the size in bytes of a chunk that should be
    /// run in parallel. If `omit_empty_entry_after_last_record` is
    /// false, the split will behave as normal `split` does, i.e. an
    /// empty record will be reported at the end if the input ends
    /// with a record separator.
    pub fn new(
        file_as_mmap: FileAsMmap,
        record_separator: u8,
        chunk_size: usize,
        omit_empty_entry_after_last_record: bool,
    ) -> Self {
        let sl: &[u8] = file_as_mmap.mmap.as_ref();
        let len = sl.len();
        let num_chunks = len.div_ceil(chunk_size);
        let mut chunks: Vec<Range<usize>> = Vec::with_capacity(num_chunks);
        for i in 0..num_chunks {
            let start = i * chunk_size;
            let end = (start + chunk_size).min(len);
            chunks.push(start..end);
        }
        Self {
            file_as_mmap,
            record_separator,
            omit_empty_entry_after_last_record,
            chunks,
        }
    }

    pub fn lines_chunks(
        &self,
    ) -> impl ParallelIterator<Item = Vec<&[u8]>> + IndexedParallelIterator
    {
        self.chunks.par_iter().map(|range| {
            let whole = self.file_as_mmap.mmap.as_ref();
            let mut lines = Vec::new();
            // index in `whole`
            let mut last_line_start = if range.start == 0 {
                0
            } else {
                if let Some(next_separator) =
                    whole[range.start..].find_byte(self.record_separator)
                {
                    range.start + next_separator + 1
                } else {
                    return lines;
                }
            };
            // Include the case where the line actually starts right
            // at the begin of the next chunk, since the chunk itself
            // won't find it.
            while last_line_start <= range.end {
                let line_end = if let Some(next_separator) =
                    whole[last_line_start..].find_byte(self.record_separator)
                {
                    last_line_start + next_separator
                } else {
                    // XXX ugh skipping all remaining chunks but yes
                    // actually correct, those return empty `lines`?
                    let end = whole.len();

                    if self.omit_empty_entry_after_last_record
                        && last_line_start == end
                    {
                        // do not add it
                        break;
                    }

                    end
                };
                lines.push(&whole[last_line_start..line_end]);
                last_line_start = line_end + 1;
            }
            lines
        })
    }
}
