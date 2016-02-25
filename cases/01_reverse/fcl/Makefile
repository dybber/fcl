include ../../shared/config.mk

build: reverse

reverse.cl: reverse.fcl
	fcl -o $@ ../../../lib/prelude.fcl reverse.fcl

reverse: reverse.c reverse.cl
	gcc $(CFLAGS) $(INCLUDE) $(LIBDIR) -o $@ reverse.c $(LIB)

run: build
	./reverse

clean:
	rm -f reverse reverse.cl
