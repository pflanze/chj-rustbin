## For 'symlinks-index'

- unit tests for StartsWith
- integration test with test-data/symlinks-index/

## For 'priorities'

- Should this be done or future?: `./done/commandline/future/` (future
  is kinda good actually? It helped me find one in here. But in
  general?)

- Parse `2.12.2024` style date format.

- Parse `Mon_21_Oct_090435_CEST_2024` style date format (code in
  cj-hours-parser) .

- With mtime > 30 days old, pin to the top (with coloring?).

- List the "class" names in the help text or via an option.

- Hierarchical tasks (subtasks) step 1: treat files prefixed with a dot as tagging their parent folder

- Hierarchical tasks (subtasks) step 2: group files in such a folder to the folder

- Error location display: show `^` in next line instead?

- Allow issue files without issue timestamp like
  `test/priorities/another OPEN{2026-1-18, importance: 1}`, perhaps
  only for directories or files tagging them. Also, name those
  timestamps throughout the code consistently.

