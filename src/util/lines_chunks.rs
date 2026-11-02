use bstr::ByteSlice;
use rayon::iter::{
    IndexedParallelIterator, IntoParallelIterator, ParallelIterator,
};

/// Parameters for `ParLinesChunks::par_lines_chunks`
///
/// `chunk_size` is the size in bytes of a chunk that should be
/// run in parallel. If `omit_empty_entry_after_last_record` is
/// false, the split will behave as normal `split` does, i.e. an
/// empty record will be reported at the end if the input ends
/// with a record separator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LinesChunksSettings {
    pub record_separator: u8,
    pub chunk_size: usize,
    pub omit_empty_entry_after_last_record: bool,
}

pub trait ParLinesChunks {
    /// Returns a parallel iterator over chunks of records (lines) of
    /// the input
    ///
    /// The splitting into chunks is trivial and done on
    /// the fly; the splitting into records is done in the iterator,
    /// i.e. for each chunk in parallel.
    fn par_lines_chunks(
        &self,
        settings: LinesChunksSettings,
    ) -> impl ParallelIterator<Item = Vec<&[u8]>> + IndexedParallelIterator;
}

impl ParLinesChunks for [u8] {
    fn par_lines_chunks(
        &self,
        settings: LinesChunksSettings,
    ) -> impl ParallelIterator<Item = Vec<&[u8]>> + IndexedParallelIterator
    {
        let whole: &[u8] = self;
        let len = whole.len();
        let num_chunks = len.div_ceil(settings.chunk_size);
        (0..num_chunks).into_par_iter().map(move |i| {
            let start = i * settings.chunk_size;
            let end = (start + settings.chunk_size).min(len);
            let range = start..end;

            let mut lines = Vec::new();
            // index in `whole`
            let mut last_line_start = if range.start == 0 {
                0
            } else {
                if let Some(next_separator) =
                    whole[range.start..].find_byte(settings.record_separator)
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
                let line_end = if let Some(next_separator) = whole
                    [last_line_start..]
                    .find_byte(settings.record_separator)
                {
                    last_line_start + next_separator
                } else {
                    // XXX ugh skipping all remaining chunks but yes
                    // actually correct, those return empty `lines`?
                    let end = whole.len();

                    if settings.omit_empty_entry_after_last_record
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
