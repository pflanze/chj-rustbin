/// Make a delimited for temporary objects. Unlike `{ ... }` which is
/// not enough, this macro also includes a `;` which does the
/// trick. Useful e.g. to limit the scope of Mutex or RefCell guards.
#[macro_export]
macro_rules! scope {
    { $($code:tt)* } => {{
        let res = {
            $($code)*
        };
        res
    }}
}
