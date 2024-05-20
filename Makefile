all:
	cargo build --release

test_intersection: target/release/intersection
	test/run

test: test_intersection
	cargo test --release

target/release/%: src/bin/%.rs src/*.rs
	cargo build --release
	touch $@
# ^ touch because cargo won't update binaries if they don't depend on
# a src/*.rs file; and then running make as root again runs cargo
# again, which is rebuilding everything.

/usr/local/bin/lastitem: target/release/lastitem
	install target/release/lastitem /usr/local/bin/lastitem
	rm -f /usr/local/bin/lastfile && ln -s -r /usr/local/bin/lastitem /usr/local/bin/lastfile
	rm -f /usr/local/bin/lastdir && ln -s -r /usr/local/bin/lastitem /usr/local/bin/lastdir

/usr/local/bin/%: target/release/%
	install $< $@


install: /usr/local/bin/lastitem /usr/local/bin/symlinks-index /usr/local/bin/e /usr/local/bin/intersection

.PHONY: test test_intersection
