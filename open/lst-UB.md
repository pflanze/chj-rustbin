There is UB somewhere in `lst`, it crashes when hitting it with many
files via `--find` on some machines. 

Can't find anything using Miri. Try with ASAN or Valgrind?

Or give in and replace custom allocator with bumpalo, at least to try.
