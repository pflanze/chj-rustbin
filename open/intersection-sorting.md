Now that the `intersection` tool's default unsorted handler has been
moved to `&[u8]` and `&BStr`: does it still at least *attempt* to sort
entries as UTF-8?  What does bstr do?

Because if not, then printing isn't in the desired order for `--set`.

(The `--sorted` mode is not currently affected by this since it still
uses `String`, but it would be good to move that to BStr as well if
that is workable re sorting.)
