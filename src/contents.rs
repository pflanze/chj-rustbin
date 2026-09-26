use std::{
    ops::{Deref, Range},
    sync::Arc,
};

use once_cell::sync::OnceCell;

use crate::position::{Position64, NON_ZERO_U32_ONE};

pub const DEFAULT_BLOCK_SIZE_IN_BYTES: usize = 128;

/// Saturates the value (i.e. no more than u32::MAX newlines can be
/// reflected in an input)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct TerminatorCountAtEndOfBlock(u32);

/// Index of line terminators
///
/// `BLOCK_SIZE_IN_BYTES` specifies how many bytes of the source input
/// is taken for one `TerminatorCountAtEndOfBlock` value.
///
#[derive(Debug)]
struct TerminatorIndex<const BLOCK_SIZE_IN_BYTES: usize> {
    line_terminator: u8,
    blocks: Box<[OnceCell<TerminatorCountAtEndOfBlock>]>,
}

impl<const BLOCK_SIZE_IN_BYTES: usize> TerminatorIndex<BLOCK_SIZE_IN_BYTES> {
    fn new(line_terminator: u8, content: &[u8]) -> Self {
        // Do not need a cache for the last unfinished block
        let num_blocks = content.len() / BLOCK_SIZE_IN_BYTES;
        let blocks = (0..num_blocks)
            .map(|_| OnceCell::new())
            .collect::<Vec<_>>()
            .into();
        Self {
            line_terminator,
            blocks,
        }
    }

    /// Which blocks are evaluated; meant for debugging only.
    // Used in tests
    #[allow(unused)]
    fn blocks_evaluated(&self) -> Vec<bool> {
        self.blocks
            .iter()
            .map(|cell| cell.get().is_some())
            .collect()
    }

    fn count_line_terminators_opt(
        line_terminator: u8,
        content: &[u8; BLOCK_SIZE_IN_BYTES],
    ) -> u32 {
        let mut count: u32 = 0;
        for b in content {
            if *b == line_terminator {
                count = count.saturating_add(1);
            }
        }
        count
    }

    // XX todo: make tail recursive
    /// `block_index` must always be for a full block, panics otherwise!
    fn terminator_count_at_end_of_block(
        &self,
        block_index: usize,
        content: &Box<[u8]>,
    ) -> TerminatorCountAtEndOfBlock {
        *self.blocks[block_index].get_or_init(|| {
            let start = BLOCK_SIZE_IN_BYTES * block_index;
            let end = BLOCK_SIZE_IN_BYTES * (block_index + 1);
            let sl = &content[start..end.min(content.len())];
            let arr = <&[u8; BLOCK_SIZE_IN_BYTES]>::try_from(sl).expect(
                "never asking for the terminator count at the end of the \
                 last non-block-filling part",
            );
            let c = Self::count_line_terminators_opt(self.line_terminator, arr);
            let prev_c = block_index
                .checked_sub(1)
                .map(|prev_block_index| {
                    self.terminator_count_at_end_of_block(
                        prev_block_index,
                        content,
                    )
                })
                .unwrap_or(TerminatorCountAtEndOfBlock(0));
            TerminatorCountAtEndOfBlock(prev_c.0.saturating_add(c))
        })
    }

    // XX todo: make tail recursive
    fn column_at_end_of_block(
        &self,
        block_index: usize,
        content: &Box<[u8]>,
    ) -> usize {
        let start = BLOCK_SIZE_IN_BYTES * block_index;
        let end = BLOCK_SIZE_IN_BYTES * (block_index + 1);
        let sl = &content[start..end.min(content.len())];
        sl.iter()
            .rev()
            .position(|b| *b == self.line_terminator)
            .unwrap_or_else(|| {
                if let Some(prev_block_index) = block_index.checked_sub(1) {
                    sl.len()
                        + self.column_at_end_of_block(prev_block_index, content)
                } else {
                    sl.len()
                }
            })
    }

    /// Panics if `index` is out of bounds of `content`
    // XX todo: make tail recursive
    fn position_at(&self, index: usize, content: &Box<[u8]>) -> Position64 {
        let block_index = index / BLOCK_SIZE_IN_BYTES;
        let start = BLOCK_SIZE_IN_BYTES * block_index;
        // let end = BLOCK_SIZE_IN_BYTES * (block_index + 1);
        let sl = &content[start..index];
        let pos_within_sl = Position64::from_run_up(sl, self.line_terminator);
        let (terminator_count_at_end_of_previous_block, opt_prev_column) =
            block_index
                .checked_sub(1)
                .map(|previous_block_index| {
                    (
                        self.terminator_count_at_end_of_block(
                            previous_block_index,
                            content,
                        ),
                        None,
                    )
                })
                .unwrap_or((TerminatorCountAtEndOfBlock(0), Some(0)));

        let prev_column = if pos_within_sl.line == NON_ZERO_U32_ONE {
            // Need previous column value
            opt_prev_column.unwrap_or_else(|| {
                self.column_at_end_of_block(block_index - 1, content) as u32
            })
        } else {
            // `+` below won't use this value, just use anything
            0
        };
        Position64 {
            line: NON_ZERO_U32_ONE
                .saturating_add(terminator_count_at_end_of_previous_block.0),
            column: prev_column,
        } + pos_within_sl
    }
}

#[derive(Debug)]
pub struct BackingWithIndex<const BLOCK_SIZE_IN_BYTES: usize> {
    content: Box<[u8]>,
    index: TerminatorIndex<BLOCK_SIZE_IN_BYTES>,
}

impl<const BLOCK_SIZE_IN_BYTES: usize> PartialEq
    for BackingWithIndex<BLOCK_SIZE_IN_BYTES>
{
    fn eq(&self, other: &Self) -> bool {
        self.content == other.content
    }
}

impl<const BLOCK_SIZE_IN_BYTES: usize> Eq
    for BackingWithIndex<BLOCK_SIZE_IN_BYTES>
{
}

impl<const BLOCK_SIZE_IN_BYTES: usize> Deref
    for BackingWithIndex<BLOCK_SIZE_IN_BYTES>
{
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl<const BLOCK_SIZE_IN_BYTES: usize> BackingWithIndex<BLOCK_SIZE_IN_BYTES> {
    pub fn new(content: Box<[u8]>, line_terminator: u8) -> Self {
        let index = TerminatorIndex::new(line_terminator, &content);
        Self { content, index }
    }

    pub fn to_contents(self: &Arc<Self>) -> Contents<BLOCK_SIZE_IN_BYTES> {
        let range = 0..self.len();
        Contents {
            backing: self.clone(),
            range,
        }
    }

    fn as_slice(&self) -> &[u8] {
        &self.content
    }

    pub fn line_terminator(&self) -> u8 {
        self.index.line_terminator
    }

    pub fn position_at(&self, index: usize) -> Position64 {
        self.index.position_at(index, &self.content)
    }
}

/// Kind of like a slice into a contents backing storage, but
/// "ownership-shared" via `Arc`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Contents<
    const BLOCK_SIZE_IN_BYTES: usize = DEFAULT_BLOCK_SIZE_IN_BYTES,
> {
    backing: Arc<BackingWithIndex<BLOCK_SIZE_IN_BYTES>>,
    range: Range<usize>,
}

#[test]
fn t_size_contents() {
    assert_eq!(size_of::<Contents<100>>(), 3 * size_of::<usize>());
}

// No need, Deref works!
// impl Index<Range<usize>> for Contents {
// }

impl<const BLOCK_SIZE_IN_BYTES: usize> Deref for Contents<BLOCK_SIZE_IN_BYTES> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl<const BLOCK_SIZE_IN_BYTES: usize> Contents<BLOCK_SIZE_IN_BYTES> {
    pub fn new_full(content: Box<[u8]>, line_terminator: u8) -> Self {
        let range = 0..content.len();
        Self {
            backing: Arc::new(BackingWithIndex::new(content, line_terminator)),
            range,
        }
    }

    pub fn backing(&self) -> &Arc<BackingWithIndex<BLOCK_SIZE_IN_BYTES>> {
        &self.backing
    }

    pub fn range_in_backing(&self) -> Range<usize> {
        self.range.clone()
    }

    pub fn line_terminator(&self) -> u8 {
        self.backing.line_terminator()
    }

    pub fn as_slice(&self) -> &[u8] {
        let Self { backing, range } = self;
        &backing[range.clone()]
    }

    /// Returns the lines without the line endings, referencing via
    /// Arc clone into self's content.
    pub fn lines(
        &self,
    ) -> impl Iterator<Item = Contents<BLOCK_SIZE_IN_BYTES>> + '_ {
        let Self { backing, range } = self;
        let line_terminator = backing.line_terminator();
        let mut range_start_position = self.start_position();
        let mut range_start = range.start;
        self.as_slice()
            .split(move |b| *b == line_terminator)
            .map(move |line| {
                let this = Self {
                    backing: backing.clone(),
                    range: range_start..(range_start + line.len()),
                };
                range_start =
                    range_start.saturating_add(line.len().saturating_add(1));
                range_start_position.inc_line();
                this
            })
    }

    /// Position of the start of this contents window within the
    /// backing text
    pub fn start_position(&self) -> Position64 {
        let Self { backing, range } = self;
        backing.position_at(range.start)
    }

    pub fn position_at(&self, offset_within_window: usize) -> Position64 {
        let Self { backing, range } = self;
        backing.position_at(range.start + offset_within_window)
    }
}

#[cfg(test)]
mod tests {
    use crate::position::position64;

    use super::*;

    #[test]
    fn t_() {
        let pos = position64;

        let text = b"Hi!\n\
                     How are you?\n\
                     Good.\n\
                     Perfect!\n";

        {
            let bwi = BackingWithIndex::<10>::new(text.to_vec().into(), b'\n');
            let c = |block_index: usize| {
                bwi.index
                    .terminator_count_at_end_of_block(block_index, &bwi.content)
                    .0
            };
            assert_eq!(c(1), 2);
            assert_eq!(c(0), 1);
            assert_eq!(c(2), 3);
            let e = std::panic::catch_unwind(|| c(3)).err().unwrap();
            assert_eq!(
                e.downcast_ref::<String>().unwrap(),
                "index out of bounds: the len is 3 but the index is 3"
            );

            let c = |block_index: usize| {
                bwi.index.column_at_end_of_block(block_index, &bwi.content)
            };
            assert_eq!(c(0), 6);
            assert_eq!(c(1), 3);
            assert_eq!(c(2), 7);
        }

        {
            let b = |n: usize| {
                let bwi = BackingWithIndex::<10>::new(
                    text[0..n].to_vec().into(),
                    b'\n',
                );
                (bwi.index.blocks.len(), bwi.position_at(n))
            };
            assert_eq!(b(0), (0, pos(1, 0)));
            assert_eq!(b(9), (0, pos(2, 5)));
            assert_eq!(b(10), (1, pos(2, 6)));
            assert_eq!(b(29), (2, pos(4, 6)));
            assert_eq!(b(30), (3, pos(4, 7)));
            assert_eq!(b(32), (3, pos(5, 0)));
        }

        {
            let bwi = BackingWithIndex::<10>::new(text.to_vec().into(), b'\n');
            assert_eq!(&bwi[0..5], b"Hi!\nH");
            assert_eq!(bwi.position_at(0), pos(1, 0));
            assert_eq!(bwi.position_at(3), pos(1, 3));
            assert_eq!(bwi.position_at(4), pos(2, 0));
            assert_eq!(bwi.position_at(8), pos(2, 4));

            assert_eq!(bwi.index.blocks_evaluated(), [false, false, false]);

            assert_eq!(bwi.position_at(10), pos(2, 6));
            assert_eq!(bwi.index.blocks_evaluated(), [true, false, false]);

            assert_eq!(bwi.position_at(19), pos(3, 2));
            assert_eq!(bwi.index.blocks_evaluated(), [true, false, false]);
            assert_eq!(bwi.position_at(20), pos(3, 3));
            assert_eq!(bwi.index.blocks_evaluated(), [true, true, false]);

            assert_eq!(bwi.position_at(29), pos(4, 6));
            assert_eq!(bwi.index.blocks_evaluated(), [true, true, false]);
            assert_eq!(bwi.position_at(30), pos(4, 7));
            assert_eq!(bwi.index.blocks_evaluated(), [true, true, true]);

            assert_eq!(bwi.position_at(bwi.len()), pos(5, 0));
        }
    }
}
