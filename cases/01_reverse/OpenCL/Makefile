INCLUDE = -I../../../microcl/
LIBDIR = -L../../../microcl/
CFLAGS=-Wall -O2 -std=c99 -D_DEBUG 
LIB = -lmcl -lOpenCL

build: reverse

reverse: reverse.c
	gcc $(CFLAGS) $(INCLUDE) $(LIBDIR) -o $@ $^ $(LIB)

run: build
	./reverse -k0
	./reverse -k1

clean:
	rm reverse
