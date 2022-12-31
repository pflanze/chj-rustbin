all:
	cargo build --release


target/release/lastitem: src/bin/lastitem.rs
	cargo build --release

/usr/local/bin/lastitem: target/release/lastitem
	install target/release/lastitem /usr/local/bin/lastitem
	rm -f /usr/local/bin/lastfile && ln -s -r /usr/local/bin/lastitem /usr/local/bin/lastfile
	rm -f /usr/local/bin/lastdir && ln -s -r /usr/local/bin/lastitem /usr/local/bin/lastdir


target/release/symlinks-index: src/startswith.rs src/bin/symlinks-index.rs
	cargo build --release

/usr/local/bin/symlinks-index: target/release/symlinks-index
	install target/release/symlinks-index /usr/local/bin/symlinks-index


target/release/e: src/bin/e.rs src/rawfdreader.rs
	cargo build --release

/usr/local/bin/e: target/release/e
	install target/release/e /usr/local/bin/e


install: /usr/local/bin/lastitem /usr/local/bin/symlinks-index /usr/local/bin/e

