INCLUDE = -I../../../microcl/
LIBDIR = -L../../../microcl/
CFLAGS=-Wall -O2 -std=c99 -D_DEBUG 
LIB = -lmcl -lOpenCL

build: copy

copy: copy.c
	gcc $(CFLAGS) $(INCLUDE) $(LIBDIR) -o $@ $^ $(LIB)

run: build
	./copy

clean:
	rm -f copy
