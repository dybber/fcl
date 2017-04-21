include ../../shared/config.mk

build: matrixMul # matrixMul_profile

matrixMul: matrixMul.c
	gcc $(CFLAGS) $(INCLUDE) $(LIBDIR) -o $@ $^ $(LIB)

# matrixMul_profile: matrixMul_profile.c
# 	gcc $(CFLAGS) $(INCLUDE) $(LIBDIR) -o $@ $^ $(LIB)

run: build
	./matrixMul
#	./matrixMul_profile

clean:
	rm -f matrixMul matrixMul_profile
