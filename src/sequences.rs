use genawaiter::rc::Gen;

/// Build groups of items from the input stream. A group finishes when
/// `belong`, being passed the previous and new item, returns
/// false. Each group is first collected in a Vec, then passed to the
/// `construct` function as a mutable reference via an `Option` (which
/// is always `Some`), and the return value becomes the item in the
/// resulting sequence. The reason the vector is passed via `Option`
/// is so that `construct` can take it out via `.take().unwrap()` if
/// it wishes; if it does, `group` creates a new Vec for the next
/// group, otherwise it reuses the old one for efficiency.

/// The resulting iterator is empty (no group is reported) if the
/// input is empty.

pub fn group<T, G>(
    mut inp: impl Iterator<Item = T>,
    belong: impl Fn(&T, &T) -> bool,
    construct: impl Fn(&mut Option<Vec<T>>) -> G,
) -> impl Iterator<Item = G> {
    Gen::new(|co| async move {
        let mut v = Some(Vec::new());
        let mut last_item = None;
        while let Some(item) = inp.next() {
            if let Some(last) = last_item.take() {
                let same = belong(&last, &item);
                v.as_mut().unwrap().push(last);
                if !same {
                    co.yield_(construct(&mut v)).await;
                    if let Some(vr) = v.as_mut() {
                        vr.clear();
                    } else {
                        v = Some(Vec::new());
                    }
                }
            }
            last_item = Some(item);
        }
        if let Some(last) = last_item.take() {
            v.as_mut().unwrap().push(last);
            co.yield_(construct(&mut v)).await;
        }
    })
    .into_iter()
}

/// Same as `group` but handles errors in the input stream, making it
/// easier to use (and more efficient?) than group in that case. Also,
/// continues building the group, so in case the receiver of the
/// output stream continues to read past errors, they will still
/// receive groups, and erros do not break up groups (this would not
/// be possible to achieve via `group`).
pub fn try_group<T, G, E>(
    mut inp: impl Iterator<Item = Result<T, E>>,
    belong: impl Fn(&T, &T) -> bool,
    construct: impl Fn(&mut Option<Vec<T>>) -> G,
) -> impl Iterator<Item = Result<G, E>> {
    Gen::new(|co| async move {
        let mut v = Some(Vec::new());
        let mut last_item = None;
        while let Some(result_item) = inp.next() {
            match result_item {
                Ok(item) => {
                    if let Some(last) = last_item.take() {
                        let same = belong(&last, &item);
                        v.as_mut().unwrap().push(last);
                        if !same {
                            co.yield_(Ok(construct(&mut v))).await;
                            if let Some(vr) = v.as_mut() {
                                vr.clear();
                            } else {
                                v = Some(Vec::new());
                            }
                        }
                    }
                    last_item = Some(item);
                }
                Err(e) => co.yield_(Err(e)).await,
            }
        }
        if let Some(last) = last_item.take() {
            v.as_mut().unwrap().push(last);
            co.yield_(Ok(construct(&mut v))).await;
        }
    })
    .into_iter()
}
