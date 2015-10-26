INCLUDE = -I../../shared/include/ -I../shared/include/
LIBDIR = -L../../shared/lib/
COMPILERFLAGS = -Wall -O2
CFLAGS = $(COMPILERFLAGS) $(INCLUDE)
LIB = -lclu_runtime -lOpenCL -lstdc++ # We need stdc++ because of CLU

reduce0: reduce0.c reduce0.cl
	gcc $(CFLAGS) -D_DEBUG -std=c99 -c -o reduce0.o reduce0.c
	g++ $(CFLAGS) $(LIBDIR) -o $@ reduce0.o $(LIB) # We need g++ for linking because of CLU

build: reduce0

run: build
	./reduce0

clean:
	rm -f reduce0.o reduce0
