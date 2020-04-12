.PHONY: sandbox deps

cabal.sandbox.config:
	cabal sandbox init

deps: cabal.sandbox.config
	cabal install --dependencies-only

build: deps
	cabal configure
	cabal build

test: deps
	cabal test