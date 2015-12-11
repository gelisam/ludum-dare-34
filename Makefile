# requires the Haste compiler
#   http://haste-lang.org/downloads/

all: init build
.PHONY: all init test

init: .cabal-sandbox/
.cabal-sandbox:
	haste-cabal sandbox init

test: build
	open $<

build: .cabal-sandbox/bin/ludum-dare34.html
.cabal-sandbox/bin/ludum-dare34.html: src/Main.hs
	haste-cabal install