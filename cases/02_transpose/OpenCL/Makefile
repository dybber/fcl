INCLUDE = -I../../../microcl/
LIBDIR = -L../../../microcl/
CFLAGS=-Wall -O2 -std=c99 -D_DEBUG 
LIB = -lmcl -lOpenCL

build: transpose

transpose: transpose.c
	gcc $(CFLAGS) $(INCLUDE) $(LIBDIR) -o $@ $^ $(LIB)

run: build
	./transpose

clean:
	rm -f transpose
