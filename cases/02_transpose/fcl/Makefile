include ../../shared/config.mk

build: transpose

transpose.cl: transpose.fcl
	fcl -o $@ ../../../lib/prelude.fcl transpose.fcl

transpose: transpose.c transpose.cl
	gcc $(CFLAGS) $(INCLUDE) $(LIBDIR) -o $@ transpose.c $(LIB)

run: build
	./transpose

clean:
	rm -f transpose transpose.cl
