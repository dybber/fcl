INCLUDE = -I../microcl/ -I../include/ -I/usr/local/cuda/include/
LIBDIR = -L../microcl/
CFLAGS=-Wall -O2 -std=c99 -D_DEBUG
LIB=-lmcl -lOpenCL

build: reduce

Reduce:
	ghc Reduce.hs 

main.c: Reduce
	./Reduce

reduce: main.c kernels.cl
	gcc $(CFLAGS) $(INCLUDE) $(LIBDIR) -o $@ main.c $(LIB)

run: build
	./reduce

clean:
	rm -f Reduce kernels.cl main.c
