usage:
	@echo "use 'make install', 'make build', 'make build-cases', 'make clean'"

install:
	$(MAKE) -C microcl all
	cabal install

build:
	$(MAKE) -C microcl all
	cabal build

build-cases: build-fcl-cases build-opencl-cases

benchmark: benchmark-fcl benchmark-opencl

build-fcl-cases: install
	$(MAKE) -C cases/ build-fcl

build-opencl-cases: install
	$(MAKE) -C cases/ build-opencl

benchmark-fcl: build-cases
	$(MAKE) -C cases/ run-fcl

benchmark-opencl: build-cases
	$(MAKE) -C cases/ run-opencl

clean-cases:
	$(MAKE) -C cases/ clean
