# Christian's Rust binaries

This is a collection of programs that I use in everyday computing life
on the command line. I use Linux (Debian), and am not normally testing
them on macOS, and I know there are some places that won't work on
Windows, although adapting shouldn't be hard (in most cases).

I used to write such programs in Perl or Bash in
[chj-scripts](https://github.com/pflanze/chj-scripts), and still do
for things where those technologies fit better. But the Rust programs
will certainly be of better quality; replacements for the original
scripts (like `e`, `duu`) have left their originals unmaintained. But
most programs here are completely new.

I might move some of the programs to their own crates and publish them
on crates.io if interested.

Also included is a number of crates (some published on crates.io) that
I started to support those programs.

## Install

Everything builds using `cargo` just fine. But there is also a
[`Makefile`](Makefile) to run the integration tests (`make test`),
test under Miri (`make miri`), and build statically on Debian (`make
all-musl`, but see below). I install the programs under
`/usr/local/bin` and they need some symlinks, `make install` or `make
install-musl` does that (but `make`, or `make all-musl`, respectively,
has to be run explicitly first).

### Static builds via musl

`cargo` can make static builds on Linux easily when the corresponding
musl target has been installed via `rustup`. But since I rely on
Debian to get the rust compiler and cargo (via
[chjize](https://github.com/pflanze/chjize)), I didn't want to then
install musl via `rustup` when it came to doing the static
builds. Instead, the `all-musl` make target does the build using a
musl installed in `$HOME/opt/libunwind-musl/`. libunwind is needed
since musl doesn't satisfy the needs of the LLVM-generated code when
it comes to handle panics, and acts as a stand-in. But the
`libunwind-dev` package provided by Debian sadly is built for glibc,
not musl, thus can't be linked. I thus use this approach to build
libunwind for musl (versions and paths for Debian trixie):

1. Get the libunwind sources via Debian (actually gets the whole llvm-toolchain):

        apt-get source libunwind-22-dev

2. Build and install libunwind directly, bypassing dpkg (avoids having
   to install all of the build dependencies for building the Debian
   package, and the goal is to compile it differently anyway):

        cmake -S llvm-toolchain-22-22.1.8/runtimes -B build-musl \
          -DLLVM_ENABLE_RUNTIMES=libunwind \
          -DCMAKE_C_COMPILER=/usr/lib/llvm-19/bin/clang \
          -DCMAKE_CXX_COMPILER=/usr/lib/llvm-19/bin/clang++ \
          -DCMAKE_C_COMPILER_TARGET=x86_64-unknown-linux-musl \
          -DCMAKE_CXX_COMPILER_TARGET=x86_64-unknown-linux-musl \
          -DLIBUNWIND_ENABLE_SHARED=OFF \
          -DLIBUNWIND_ENABLE_STATIC=ON \
          -DLIBUNWIND_INCLUDE_TESTS=OFF \
          -DCMAKE_INSTALL_PREFIX="$HOME/opt/libunwind-musl"
        cmake --build build-musl
        cmake --build build-musl --target install

After that, `make all-musl` in chj-rustbin should work.
