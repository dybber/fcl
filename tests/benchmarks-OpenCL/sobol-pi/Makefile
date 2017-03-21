include ../../shared/config.mk

build: sobol-pi

sobol-pi: sobol-independent.c
	gcc $(CFLAGS) $(INCLUDE) $(LIBDIR) -o $@ $^ $(LIB)

run: build
	./sobol-pi

clean:
	rm -f sobol-pi
