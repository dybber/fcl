include ../../shared/config.mk

build: copy

copy: copy.c
	gcc $(CFLAGS) $(INCLUDE) $(LIBDIR) -o $@ $^ $(LIB)

run: build
	./copy

clean:
	rm -f copy
