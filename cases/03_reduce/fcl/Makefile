include ../../shared/config.mk

build: red

red.c: reduce.fcl
	fcl -o $@ reduce.fcl

red: red.c kernels.cl
	gcc $(CFLAGS) $(INCLUDE) $(LIBDIR) -o $@ red.c $(LIB)

run: build
	./red

clean:
	rm -f reduce reduce.cl kernels.cl red.c
