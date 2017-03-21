include ../../shared/config.mk

build: transpose transpose_profile

transpose: transpose.c
	gcc $(CFLAGS) $(INCLUDE) $(LIBDIR) -o $@ $^ $(LIB)

transpose_profile: transpose_profile.c
	gcc $(CFLAGS) $(INCLUDE) $(LIBDIR) -o $@ $^ $(LIB)

run: build
	./transpose
	./transpose_profile

clean:
	rm -f transpose transpose_profile
