build:
	cabal build

install:
	cabal install

build-cases: install
	$(MAKE) -C cases/ run

benchmark: build-cases
	$(MAKE) -C cases/ run
