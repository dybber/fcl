include ../../shared/config.mk

build: matrixMul

matrixMul: matrixMul.c
	gcc $(CFLAGS) $(INCLUDE) $(LIBDIR) -o $@ $^ $(LIB)

run: build
	./matrixMul

clean:
	rm -f matrixMul
