//! Copy from xmlhub-indexer's util.rs

/// Format a sequence of items that can be represented as &str to a
/// string for human consumption; e.g. `[String::from("Hi"),
/// String::from("there")]` => `"\"Hi\", \"there\""`.
pub fn format_string_list<S, L>(sequence: L) -> String
where
    S: AsRef<str>,
    L: IntoIterator<Item = S>,
{
    let iter = sequence.into_iter();
    let items: Vec<String> = iter.map(|v| format!("{:?}", v.as_ref())).collect();
    items.join(", ")
}

#[test]
fn t_format_string_list() {
    assert_eq!(
        format_string_list([String::from("Hi"), String::from("there")]),
        "\"Hi\", \"there\""
    );
}
