use std::{
    num::{NonZeroU32, NonZeroU64},
    ops::Add,
};

// Safe because 1 is non-zero.
pub const NON_ZERO_U32_ONE: NonZeroU32 =
    unsafe { NonZeroU32::new_unchecked(1) };
pub const NON_ZERO_U64_ONE: NonZeroU64 =
    unsafe { NonZeroU64::new_unchecked(1) };

/// Position within a file, limited to 32-bit values
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Position64 {
    pub line: NonZeroU32,
    pub column: u32,
}

/// Position within a file with no risk for truncation
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Position128 {
    pub line: NonZeroU64,
    pub column: u64,
}

macro_rules! impl_position_n {
    { $Position:tt, $NonZeroU:ty, $NON_ZERO_U_ONE:ident, $u:ty } => {

        impl $Position {
            /// Not called `ZERO` because it starts at line 1
            pub const TOP_LEFT: $Position = $Position {
                line: unsafe {
                    // 1 is NonZero
                    <$NonZeroU>::new_unchecked(1)
                },
                column: 0,
            };

            /// `run_up` is the contents before the
            pub fn from_run_up(run_up: &[u8], line_terminator: u8) -> $Position {
                let mut last_terminator_pos: usize = 0;
                let mut line: $u = 1;
                for (i, b) in run_up.iter().enumerate() {
                    if *b == line_terminator {
                        line = line.saturating_add(1);
                        last_terminator_pos = i + 1;
                    }
                }
                let column_usize = run_up.len() - last_terminator_pos;
                // XX how do better? saturating_into?
                let column: $u = column_usize as $u;
                $Position {
                    line: line.try_into().expect(
                        "guaranteed since started at 1 and only doing saturating add",
                    ),
                    column,
                }
            }

            pub fn inc_line(&mut self) {
                self.line = self.line.saturating_add(1);
                self.column = 0;
            }
        }

        impl Add for $Position {
            type Output = $Position;

            // XX a + b != b + a  !
            /// `rhs` is assumed to be a local position inside a window after
            /// `self` in an outer context; returns the position in that outer
            /// context.
            ///
            /// Never panics, does saturating adds.
            fn add(self, rhs: Self) -> Self::Output {
                if rhs.line == $NON_ZERO_U_ONE {
                    let Self { line, column } = self;
                    Self {
                        line,
                        column: column.saturating_add(rhs.column),
                    }
                } else {
                    Self {
                        line: self.line.saturating_add(rhs.line.get() - 1),
                        column: rhs.column,
                    }
                }
            }
        }
    }
}

impl_position_n!(Position64, NonZeroU32, NON_ZERO_U32_ONE, u32);
impl_position_n!(Position128, NonZeroU64, NON_ZERO_U64_ONE, u64);

#[test]
fn t_position_size() {
    assert_eq!(size_of::<Position64>(), 8);
    assert_eq!(size_of::<Option<Position64>>(), 8);
    assert_eq!(size_of::<Position128>(), 16);
    assert_eq!(size_of::<Option<Position128>>(), 16);
}

#[derive(Debug, thiserror::Error)]
#[error("position has a line or column value too large to be converted to Position64: {0:?}")]
pub struct PositionTruncationError(pub Position128);

impl TryFrom<Position128> for Position64 {
    type Error = PositionTruncationError;
    fn try_from(value: Position128) -> Result<Self, PositionTruncationError> {
        let Position128 { line, column } = value;

        let line: u32 = line
            .get()
            .try_into()
            .map_err(|_| PositionTruncationError(value))?;
        let column: u32 = column
            .try_into()
            .map_err(|_| PositionTruncationError(value))?;

        Ok(Position64 {
            line: line
                .try_into()
                .expect("always succeeds because line was already non-zero"),
            column: column as u32,
        })
    }
}

/// Meant for testing; panics if line == 0.
pub fn position64(line: u32, column: u32) -> Position64 {
    Position64 {
        line: line
            .try_into()
            .expect("user must give non-zero value for line"),
        column,
    }
}
