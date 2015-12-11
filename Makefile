# requires the Haste compiler
#   http://haste-lang.org/downloads/

all: init build
.PHONY: all init test build rebuild

init: .cabal-sandbox/sandbox-exists
.cabal-sandbox/sandbox-exists:
	haste-cabal sandbox init
	touch $@

test: build
	open http://127.0.0.1:8000/
	python -m SimpleHTTPServer

build: dist/Main.js
dist/Main.js: src/Main.hs
	haste-cabal install

rebuild:
	touch src/Main.hs
	$(MAKE) build
