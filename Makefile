# requires the Haste compiler
#   http://haste-lang.org/downloads/

all: init build
.PHONY: all init test build rebuild

init: .cabal-sandbox/sandbox-exists
.cabal-sandbox/sandbox-exists:
	haste-cabal sandbox init
	touch $@

test: build
	open http://127.0.0.1:8000/ludum-dare34.html
	python -m SimpleHTTPServer

build: ludum-dare34.html
ludum-dare34.html: src/Main.hs
	haste-cabal install
	cat .cabal-sandbox/bin/ludum-dare34.html > $@

rebuild:
	touch src/Main.hs
	$(MAKE) build
