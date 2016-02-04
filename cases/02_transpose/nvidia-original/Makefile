INCLUDEDIR = -I../../shared/nvidia-opencl/include -I/usr/local/cuda/include
LIBDIR = -L../../shared/nvidia-opencl/lib
CFLAGS=-Wall -m64 -O3 -DGPU_PROFILING
LIB= -lshrUtils -loclUtils -lOpenCL

build: oclTranspose

oclTranspose: obj/oclTranspose.cpp.o obj/transpose_gold.cpp.o
	g++ $(CFLAGS) $(LIBDIR) -o oclTranspose obj/oclTranspose.cpp.o obj/transpose_gold.cpp.o $(LIB)

obj:
	mkdir -p obj

obj/transpose_gold.cpp.o: src/transpose_gold.cpp obj
	g++ $(CFLAGS) $(INCLUDEDIR) -o obj/transpose_gold.cpp.o -c src/transpose_gold.cpp

obj/oclTranspose.cpp.o: src/oclTranspose.cpp obj/transpose_gold.cpp.o obj
	g++ $(CFLAGS) $(INCLUDEDIR) -o obj/oclTranspose.cpp.o -c src/oclTranspose.cpp


clean:
	rm -rf obj
	rm oclTranspose
