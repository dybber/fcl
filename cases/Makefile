CASES_FCL=00_copy 01_reverse
CASES_OPENCL=00_copy 01_reverse 02_transpose 03_reduce
CASES_OPENCL_DIRS=$(CASES_OPENCL:%=%/OpenCL/)
CASES_FCL_DIRS=$(CASES_FCL:%=%/fcl/)

build:
	@(for d in $(CASES_OPENCL_DIRS); do $(MAKE) -C $$d build; done;)
	@(for d in $(CASES_FCL_DIRS); do $(MAKE) -C $$d build; done;)

run:
	@(for d in $(CASES_OPENCL_DIRS); do $(MAKE) -C $$d run; done;)
	@(for d in $(CASES_FCL_DIRS); do $(MAKE) -C $$d run; done;)

clean:
	@(for d in $(CASES_OPENCL_DIRS); do $(MAKE) -C $$d clean; done;)
	@(for d in $(CASES_FCL_DIRS); do $(MAKE) -C $$d clean; done;)

