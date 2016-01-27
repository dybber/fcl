include ../../shared/config.mk

build: reverse

reverse: reverse.c
	gcc $(CFLAGS) $(INCLUDE) $(LIBDIR) -o $@ $^ $(LIB)

run: build
	./reverse -k0
	./reverse -k1

clean:
	rm reverse
