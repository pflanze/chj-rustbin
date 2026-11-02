[util/lines_chunks.rs](../src/util/lines_chunks.rs) should be tested,
the handling of lines across chunk boundaries is not well thought
through.

(Could possibly use the same test, via comparison of an older version
of `intersection`, to also test
[intersection-sorting](intersection-sorting.md).)
