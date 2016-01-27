include ../../shared/config.mk

build: transpose

transpose: transpose.c
	gcc $(CFLAGS) $(INCLUDE) $(LIBDIR) -o $@ $^ $(LIB)

run: build
	./transpose

clean:
	rm -f transpose
