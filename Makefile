all:
	cargo build --release

test_intersection: target/release/intersection
	test/intersection-run

test_priorities: target/release/priorities
	test/priorities-run

test: test_intersection test_priorities
	cargo test --release

target/release/%: src/bin/%.rs src/*.rs src/io/*.rs src/parse/*.rs src/text/*.rs src/time/*.rs src/util/*.rs
	cargo build --release
	touch $@
# ^ touch because cargo won't update binaries if they don't depend on
# a src/*.rs file; and then running make as root again runs cargo
# again, which is rebuilding everything.

/usr/local/bin/lastitem: target/release/lastitem
	install target/release/lastitem /usr/local/bin/lastitem
	rm -f /usr/local/bin/lastfile && ln -s -r /usr/local/bin/lastitem /usr/local/bin/lastfile
	rm -f /usr/local/bin/lastdir && ln -s -r /usr/local/bin/lastitem /usr/local/bin/lastdir

/usr/local/bin/e: target/release/e
	install target/release/e /usr/local/bin/e
	rm -f /usr/local/bin/f && ln -s -r /usr/local/bin/e /usr/local/bin/f
	rm -f /usr/local/bin/v && ln -s -r /usr/local/bin/e /usr/local/bin/v
	rm -f /usr/local/bin/eg && ln -s -r /usr/local/bin/e /usr/local/bin/eg
	rm -f /usr/local/bin/vg && ln -s -r /usr/local/bin/e /usr/local/bin/vg

/usr/local/bin/%: target/release/%
	install $< $@


install: /usr/local/bin/lastitem
install: /usr/local/bin/symlinks-index
install: /usr/local/bin/e
install: /usr/local/bin/intersection
install: /usr/local/bin/priorities

.PHONY: test test_intersection
