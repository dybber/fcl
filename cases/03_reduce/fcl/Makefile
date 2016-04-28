include ../../shared/config.mk

build: reduce

reduce.cl: reduce.fcl
	fcl -o $@ reduce.fcl

reduce: reduce.c reduce.cl
	gcc $(CFLAGS) $(INCLUDE) $(LIBDIR) -o $@ reduce.c $(LIB)

run: build
	./reduce

clean:
	rm -f reduce reduce.cl
