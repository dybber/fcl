include ../../shared/config.mk

build: copy

copy.cl: copy.fcl
	fcl -o $@ ../../../lib/prelude_monomorphic.fcl copy.fcl

copy: copy.c copy.cl
	gcc $(CFLAGS) $(INCLUDE) $(LIBDIR) -o $@ copy.c $(LIB)

run: build
	./copy

clean:
	rm -f copy copy.cl
