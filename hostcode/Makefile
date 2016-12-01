INCLUDE = -I../microcl/ -I../include/ -I/usr/local/cuda/include/
LIBDIR = -L../microcl/
CFLAGS=-Wall -O2 -std=c99 -D_DEBUG
LIB=-lmcl -lOpenCL

build: reduce

Reduce: Reduce.hs
	ghc Reduce.hs 

main.c: Reduce
	./Reduce

kernels.cl: Reduce
	./Reduce

reduce: main.c
	gcc $(CFLAGS) $(INCLUDE) $(LIBDIR) -o $@ main.c $(LIB)

run: build
	./reduce

clean:
	rm -f Reduce kernels.cl main.c
