use std::num::{
    NonZeroI128, NonZeroI16, NonZeroI32, NonZeroI64, NonZeroI8, NonZeroIsize,
    NonZeroU128, NonZeroU16, NonZeroU32, NonZeroU64, NonZeroU8, NonZeroUsize,
};

pub trait NonZeroUtils {
    /// The contained type (i.e. what `Self::get` returns)
    type Inner;

    /// The number 1
    const ONE: Self;

    /// Construct a Self with `val` incremented by 1
    fn new_inc(val: Self::Inner) -> Self;
}

macro_rules! def_non_zero_utils {
    { $Self:ty, $Inner:ty } => {
        impl NonZeroUtils for $Self {
            type Inner = $Inner;

            const ONE: Self = unsafe {
                // Safe because 1 is not zero
                Self::new_unchecked(1)
            };

            fn new_inc(val: Self::Inner) -> Self {
                unsafe {
                    // Safe because the addition guarantees that the value is never zero
                    Self::new_unchecked(val.saturating_add(1))
                }
            }
        }
    }
}

def_non_zero_utils!(NonZeroU8, u8);
def_non_zero_utils!(NonZeroU16, u16);
def_non_zero_utils!(NonZeroU32, u32);
def_non_zero_utils!(NonZeroU64, u64);
def_non_zero_utils!(NonZeroU128, u128);

def_non_zero_utils!(NonZeroUsize, usize);

def_non_zero_utils!(NonZeroI8, i8);
def_non_zero_utils!(NonZeroI16, i16);
def_non_zero_utils!(NonZeroI32, i32);
def_non_zero_utils!(NonZeroI64, i64);
def_non_zero_utils!(NonZeroI128, i128);

def_non_zero_utils!(NonZeroIsize, isize);
