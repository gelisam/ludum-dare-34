# requires the Haste compiler
#   http://haste-lang.org/downloads/

all: build
.PHONY: all init test build rebuild deploy

init: .cabal-sandbox/sandbox-exists
.cabal-sandbox/sandbox-exists:
	haste-cabal sandbox init
	touch $@

test: build
	open http://127.0.0.1:8000/
	python -m SimpleHTTPServer

build: dist/Main.js
dist/Main.js: .cabal-sandbox/sandbox-exists src/*.hs
	haste-cabal install

rebuild:
	touch src/Main.hs
	$(MAKE) build

deploy: build
	scp dist/Main.js `cat deploy_target`
