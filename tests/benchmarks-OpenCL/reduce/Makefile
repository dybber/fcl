include ../../shared/config.mk

build: reduce

reduce: reduce.c
	gcc $(CFLAGS) $(INCLUDE) $(LIBDIR) -o $@ $^ $(LIB)

run: build
	./reduce -k0
	./reduce -k1
	./reduce -k2
	./reduce -k3
	./reduce -k4
	./reduce -k5
	./reduce -k6

clean:
	rm -f reduce
