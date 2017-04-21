INCLUDEDIR = -I../../../shared/nvidia-opencl/include -Isrc -I/usr/local/cuda/include/
LIBDIR = -L../../../shared/nvidia-opencl/lib -L/usr/local/cuda/lib64/
CFLAGS=-Wall -m64 -O3 -DUNIX
LIB= -lshrUtils -loclUtils -lOpenCL

build: # obj/oclMatrixMul.cpp.o obj/matrixMul_gold.cpp.o
	g++ $(CFLAGS) $(INCLUDEDIR) $(LIBDIR) -o oclMatrixMul src/oclMatrixMul.cpp src/matrixMul_gold.cpp $(LIB)

obj:
	mkdir -p obj

# obj/matrixMul_gold.cpp.o: src/matrixMul_gold.cpp obj
# 	g++ $(CFLAGS) $(INCLUDEDIR) -o obj/matrixMul_gold.cpp.o -c src/matrixMul_gold.cpp

# obj/oclMatrixMul.cpp.o: src/oclMatrixMul.cpp obj
# 	g++ $(CFLAGS) $(INCLUDEDIR) -o obj/oclMatrixMul.cpp.o -c src/oclMatrixMul.cpp src/matrixMul_gold.cpp


clean:
	rm -rf obj
