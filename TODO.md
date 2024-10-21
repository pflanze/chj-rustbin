## symlinks-index

- unit tests for StartsWith
- integration test with test-data/symlinks-index/

## priorities

- Make integration test not depend on the mtime values of my local checkout.
- The string OPEN must only be matched with word boundaries before and after.
- Ignore files that also have a "DONE" tag
- Parse "2.12.2024" (and "Mon_21_Oct_090435_CEST_2024", code in
  cj-hours-parser) style date formats.
- With mtime > 30 days, pin to the top (with coloring?).
- List the "class" names in the help text or via an option.
- Hierarchical tasks (subtasks).
