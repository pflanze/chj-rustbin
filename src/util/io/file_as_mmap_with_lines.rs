use rayon::iter::{IndexedParallelIterator, ParallelIterator};

use crate::util::{io::file_as_mmap::FileAsMmap, lines_chunks::ParLinesChunks};

/// Thin wrapper for using trait ParLinesChunks
///
/// This is a bit pointless now.
pub struct FileAsMmapWithLines {
    pub file_as_mmap: FileAsMmap,
    pub record_separator: u8,
    pub chunk_size: usize,
    pub omit_empty_entry_after_last_record: bool,
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
        Self {
            file_as_mmap,
            record_separator,
            chunk_size,
            omit_empty_entry_after_last_record,
        }
    }

    pub fn lines_chunks(
        &self,
    ) -> impl ParallelIterator<Item = Vec<&[u8]>> + IndexedParallelIterator
    {
        self.file_as_mmap.mmap.as_ref().par_lines_chunks(
            self.record_separator,
            self.chunk_size,
            self.omit_empty_entry_after_last_record,
        )
    }
}
