## For 'symlinks-index'

- unit tests for StartsWith
- integration test with test-data/symlinks-index/

## For 'priorities'

- The strings OPEN or TODO must only be matched with word boundaries before and after.

- Don't just use the current year to complete dates without year.

- Parse `2.12.2024` style date format.

- Parse `Mon_21_Oct_090435_CEST_2024` style date format (code in
  cj-hours-parser) .

- With mtime > 30 days, pin to the top (with coloring?).

- List the "class" names in the help text or via an option.

- Hierarchical tasks (subtasks).
