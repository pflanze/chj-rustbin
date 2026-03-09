use std::cmp::Ordering;

pub fn merge<T>(
    a: Vec<T>,
    b: Vec<T>,
    cmp: impl for<'a, 'b, 'i> Fn(&'a T, &'b T) -> Ordering,
) -> Vec<T> {
    let mut aiter = a.into_iter();
    let mut biter = b.into_iter();
    let mut r: Vec<T> = Vec::new();
    if let Some(a) = aiter.next() {
        if let Some(b) = biter.next() {
            let (mut cur_trailer, mut source_was_b) = match cmp(&a, &b) {
                Ordering::Less | Ordering::Equal => {
                    r.push(a);
                    (b, true)
                }
                Ordering::Greater => {
                    r.push(b);
                    (a, false)
                }
            };
            loop {
                if source_was_b {
                    if let Some(a) = aiter.next() {
                        match cmp(&cur_trailer, &a) {
                            Ordering::Less | Ordering::Equal => {
                                r.push(cur_trailer);
                                cur_trailer = a;
                                source_was_b = false;
                            }
                            Ordering::Greater => {
                                r.push(a);
                            }
                        };
                    } else {
                        r.push(cur_trailer);
                        r.extend(biter);
                        break;
                    }
                } else {
                    if let Some(b) = biter.next() {
                        match cmp(&cur_trailer, &b) {
                            Ordering::Less | Ordering::Equal => {
                                r.push(cur_trailer);
                                cur_trailer = b;
                                source_was_b = true;
                            }
                            Ordering::Greater => {
                                r.push(b);
                            }
                        };
                    } else {
                        r.push(cur_trailer);
                        r.extend(aiter);
                        break;
                    }
                }
            }
        } else {
            r.push(a);
            r.extend(aiter);
        }
    } else {
        r.extend(biter);
    }

    r
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_() {
        let t = |a: &[i32], b: &[i32]| -> Vec<i32> {
            merge(Vec::from(a), Vec::from(b), |a: &i32, b: &i32| a.cmp(b))
        };
        macro_rules! t {
            { $a:expr, $b:expr, $c:expr } => {
                assert_eq!(t(&$a, &$b), $c);
            }
        }
        t!([], [], []);
        t!([1], [], [1]);
        t!([], [2], [2]);
        t!([1], [2], [1, 2]);
        t!([1, 3], [2], [1, 2, 3]);
        t!([1, 3], [2, 4], [1, 2, 3, 4]);
        t!([1, 3, 4], [2], [1, 2, 3, 4]);
        t!([1, 3, 4], [2, 4], [1, 2, 3, 4, 4]);
        t!([1, 3, 4], [1, 2, 4], [1, 1, 2, 3, 4, 4]);
    }
}
