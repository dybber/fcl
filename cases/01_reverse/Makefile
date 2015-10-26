INCLUDE = -I../../shared/include/
LIBDIR = -L../../shared/lib/
COMPILERFLAGS = -std=c99 -Wall -O2
CFLAGS = $(COMPILERFLAGS) $(INCLUDE)
LIB = -lclu_runtime -lOpenCL -lstdc++ # We need stdc++ because of CLU

build: reverse.c
	gcc $(CFLAGS) $(LIBDIR) -o reverse $^ $(LIB)

run: build
	./reverse

clean:
	rm reverse
