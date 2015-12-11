# requires the Haste compiler
#   http://haste-lang.org/downloads/

all: init build
.PHONY: all init test build rebuild

init: .cabal-sandbox/
.cabal-sandbox:
	haste-cabal sandbox init

test: build
	open $<

build: .cabal-sandbox/bin/ludum-dare34.html .cabal-sandbox/bin/img/bird.png
.cabal-sandbox/bin/ludum-dare34.html: src/Main.hs
	haste-cabal install

.cabal-sandbox/bin/img/%.png: img/%.png
	mkdir -p .cabal-sandbox/bin/img
	cp $< $@

rebuild:
	touch src/Main.hs
	$(MAKE) build
