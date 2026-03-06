//! Yet another terminal table infrastructure
//!
//! With constant auto-detected column widths (first collects all data
//! into a region and determines the max widths, then prints), filled
//! with spaces, and left/right adjusted formatting.
//!
//! CAVEAT: does not currently consider the width of non-ASCII
//! characters. (This is no problem for `ls` output since the path is
//! the last column which is left-adjusted and has no fill
//! afterwards.)

use std::{fmt::Arguments, io::Write};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Widths<const COLUMNS: usize>([u32; COLUMNS]);

impl<const COLUMNS: usize> Default for Widths<COLUMNS> {
    fn default() -> Self {
        Self([0; COLUMNS])
    }
}

impl<const COLUMNS: usize> Widths<COLUMNS> {
    fn update_max(&mut self, new: &Self) {
        for i in 0..COLUMNS {
            if new.0[i] > self.0[i] {
                self.0[i] = new.0[i];
            }
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct YatTable<const COLUMNS: usize> {
    storage: Vec<u8>,
    /// Column widths
    rows: Vec<Widths<COLUMNS>>,
    max_widths: Widths<COLUMNS>,
}

/// A row writer: call `write_cell` exactly COLUMN times; when
/// dropped, the writer finishes writing back the changes, and panics
/// if not enough cells were written.
#[derive(Debug, PartialEq, Eq)]
pub struct YatTableWriteGuard<'t, const COLUMNS: usize> {
    table: &'t mut YatTable<COLUMNS>,
    written: usize,
    row: Widths<COLUMNS>,
}

impl<'t, const COLUMNS: usize> YatTableWriteGuard<'t, COLUMNS> {
    pub fn add_cell_fmt(&mut self, fmt: Arguments) {
        let i0 = self.table.storage.len();
        _ = self.table.storage.write_fmt(fmt);
        let i1 = self.table.storage.len();
        self.row.0[self.written] = (i1 - i0)
            .try_into()
            .expect("cells are shorter than u32::MAX");
        self.written += 1;
    }

    /// Push further content to the cell that was last added (panics if
    /// none was added).
    pub fn amend_cell_fmt(&mut self, fmt: Arguments) {
        let i0 = self.table.storage.len();
        _ = self.table.storage.write_fmt(fmt);
        let i1 = self.table.storage.len();
        let added_len: u32 = (i1 - i0)
            .try_into()
            .expect("cells are shorter than u32::MAX");
        let new_len = self.row.0[self.written - 1] + added_len;
        self.row.0[self.written - 1] = new_len;
    }

    pub fn add_cell_bytes(&mut self, bytes: &[u8]) {
        _ = self.table.storage.extend_from_slice(bytes);
        self.row.0[self.written] = bytes
            .len()
            .try_into()
            .expect("cells are shorter than u32::MAX");
        self.written += 1;
    }

    /// Push further bytes to the cell that was last added (panics if
    /// none was added).
    pub fn amend_cell_bytes(&mut self, bytes: &[u8]) {
        _ = self.table.storage.extend_from_slice(bytes);
        let added_len: u32 = bytes
            .len()
            .try_into()
            .expect("cells are shorter than u32::MAX");
        let new_len = self.row.0[self.written - 1] + added_len;
        self.row.0[self.written - 1] = new_len;
    }
}

impl<'t, const COLUMNS: usize> Drop for YatTableWriteGuard<'t, COLUMNS> {
    fn drop(&mut self) {
        if self.written != COLUMNS {
            panic!(
                "need {COLUMNS} cells per row, only got {} adds",
                self.written
            );
        }
        self.table.rows.push(self.row.clone());
        self.table.max_widths.update_max(&self.row);
    }
}

impl<const COLUMNS: usize> YatTable<COLUMNS> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn new_row(&mut self) -> YatTableWriteGuard<'_, COLUMNS> {
        YatTableWriteGuard {
            table: self,
            written: 0,
            row: Default::default(),
        }
    }

    pub fn write_out(
        &self,
        right_adjust: &[bool; COLUMNS],
        output_record_separator: u8,
        out: &mut impl Write,
    ) -> Result<(), std::io::Error> {
        const COL_SEPARATION: usize = 1;
        let max_gap_len = self.max_widths.0.iter().copied().max().unwrap_or(0)
            as usize
            + COL_SEPARATION;
        let spaces = vec![b' '].repeat(max_gap_len);
        let mut i: usize = 0;
        for row in &self.rows {
            for col in 0..COLUMNS {
                let is_last = col + 1 == COLUMNS;

                let col_width = row.0[col];
                let col_width_max = self.max_widths.0[col];
                let i1 = i + usize::try_from(col_width)
                    .expect("u32 convertible to usize");
                let bytes = &self.storage[i..i1];
                i = i1;
                let right_adjust = right_adjust[col];
                let (gap_len_before, gap_len_after) = if is_last {
                    (0, 0)
                } else {
                    let fill = usize::try_from(col_width_max - col_width)
                        .expect("u32 convertible to usize");
                    if right_adjust {
                        (fill, 1)
                    } else {
                        (0, fill + COL_SEPARATION)
                    }
                };
                if gap_len_before > 0 {
                    out.write_all(&spaces[0..gap_len_before])?;
                }
                out.write_all(bytes)?;
                if gap_len_after > 0 {
                    out.write_all(&spaces[0..gap_len_after])?;
                }
            }
            out.write_all(&[output_record_separator])?;
        }
        Ok(())
    }
}
