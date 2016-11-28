install:
	$(MAKE) -C ../cgen/ install
	$(MAKE) -C microcl all
	cabal install

build:
	$(MAKE) -C microcl all
	cabal build

clean:
	$(MAKE) -C microcl clean
	cabal clean

# Cases
benchmark: benchmark-fcl benchmark-opencl

build-cases: build-fcl-cases build-opencl-cases

clean-cases:
	$(MAKE) -C cases/ clean

build-fcl-cases: install
	$(MAKE) -C cases/ build-fcl

build-opencl-cases: install
	$(MAKE) -C cases/ build-opencl

benchmark-fcl: build-cases
	$(MAKE) -C cases/ run-fcl

benchmark-opencl: build-cases
	$(MAKE) -C cases/ run-opencl

usage:
	@echo "use 'make install', 'make build', 'make build-cases', 'make clean'"
