include ../../shared/config.mk

build: transpose

# transpose.cl: transpose.fcl
# 	fcl -o $@ transpose.fcl

transpose3.cl: transpose3.fcl
	fcl -o $@ transpose3.fcl

transpose: transpose.c transpose3.cl
	gcc $(CFLAGS) $(INCLUDE) $(LIBDIR) -o $@ transpose.c $(LIB)

run: build
	./transpose

clean:
	rm -f transpose transpose3.cl
