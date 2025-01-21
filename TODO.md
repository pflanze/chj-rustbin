## For 'symlinks-index'

- unit tests for StartsWith
- integration test with test-data/symlinks-index/

## For 'priorities'

- Don't just use the current year to complete dates without year. Use
  and require the date in the file name as base.

- Should this be done or future?: `./done/commandline/future/` (future
  is kinda good actually? It helped me find one in here. But in
  general?)

- Parse `2.12.2024` style date format.

- Parse `Mon_21_Oct_090435_CEST_2024` style date format (code in
  cj-hours-parser) .

- With mtime > 30 days, pin to the top (with coloring?).

- List the "class" names in the help text or via an option.

- Hierarchical tasks (subtasks) step 1: treat files prefixed with a dot as tagging their parent folder

- Hierarchical tasks (subtasks) step 2: group files in such a folder to the folder

- Error location display: show `^` in next line instead?
