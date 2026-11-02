- `make` has to be run explicitly before `make install` can be run. The reason
  that the `install` target does not state a dependency on `all` is that I run
  `make` as non-root user, then `make install` as root, and the `all` target
  just unconditionally runs `cargo`, hence root would run `cargo` which would
  defeat the purpose of running `make` as non-root first. I don't know how to
  improve this.
