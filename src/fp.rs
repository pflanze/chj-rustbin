/// Invert the boolean result of a function.
pub fn complement<T>(f: impl Fn(T) -> bool) -> impl Fn(T) -> bool {
    move |c: T| -> bool { !f(c) }
}

pub fn on<T, K, R>(
    access: impl Fn(&T) -> K,
    cmp: impl Fn(K, K) -> R,
) -> impl Fn(&T, &T) -> R {
    move |a: &T, b: &T| cmp(access(a), access(b))
}

pub fn compose<A, B, C>(
    f: impl Fn(A) -> B,
    g: impl Fn(B) -> C,
) -> impl Fn(A) -> C {
    move |x| g(f(x))
}
