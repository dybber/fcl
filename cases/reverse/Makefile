INCLUDE = -I../../shared/include/
LIBDIR = -L../../shared/lib/
COMPILERFLAGS = -std=c99 -Wall -O2
CFLAGS = $(COMPILERFLAGS) $(INCLUDE)
LIB = -lclu_runtime -lOpenCL

reverse: reverse.c
	g++ $(CFLAGS) $(LIBDIR) -o $@ $^ $(LIB)

clean:
	rm reverse
