// #[macro_export]
// macro_rules! alternatively {
//     { $e1:tt, $var:ident : $E:ty => $e2:tt } => {
//         (|| -> Result<_, $E> {
//             Ok($e1)
//         })().or_else(|$var: $E| -> Result<_, bool> {
//             Ok($e2)
//         })
//     }
// }

/// Try two approaches to generate `T`, if both fail, return the error
/// of the second attempt (but `f2` could embed the error of the first
/// attempt in that).
pub fn alternatively_with_error<T, E1, E2>(
    f1: impl FnOnce() -> Result<T, E1>,
    f2: impl FnOnce(E1) -> Result<T, E2>,
) -> Result<T, E2> {
    f1().or_else(f2)
}

/// Try two approaches to generate `T`, if both fail, return the error
/// of the second attempt (but `f2` could embed the error of the first
/// attempt in that). Unlike `alternatively_with_error`, this function
/// forces the error type of both closures to be the same; this is
/// less flexible but has the advantage that the error type for the
/// second closure doesn't have to be specified if it specifies it as
/// the argument.
pub fn alternatively_with_same_error_type<T, E>(
    f1: impl FnOnce() -> Result<T, E>,
    f2: impl FnOnce(E) -> Result<T, E>,
) -> Result<T, E> {
    f1().or_else(f2)
}

// want to just specify E.
// and both functions just thunks.
// Best we can do is to specify _ for T at the call site.

/// Try two argument-less functions of the same function type to
/// generate `T`, if both fail, return the error of the second attempt
/// (the first error is lost).
pub fn alternatively<T, E>(
    f1: impl FnOnce() -> Result<T, E>,
    f2: impl FnOnce() -> Result<T, E>,
) -> Result<T, E> {
    f1().or_else(|_| f2())
}
