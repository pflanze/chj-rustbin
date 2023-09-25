all:
	cargo build --release

test: target/release/intersection
	test/run

target/release/%: src/bin/%.rs src/*.rs
	cargo build --release

/usr/local/bin/lastitem: target/release/lastitem
	install target/release/lastitem /usr/local/bin/lastitem
	rm -f /usr/local/bin/lastfile && ln -s -r /usr/local/bin/lastitem /usr/local/bin/lastfile
	rm -f /usr/local/bin/lastdir && ln -s -r /usr/local/bin/lastitem /usr/local/bin/lastdir

/usr/local/bin/%: target/release/%
	install $< $@


install: /usr/local/bin/lastitem /usr/local/bin/symlinks-index /usr/local/bin/e /usr/local/bin/intersection

