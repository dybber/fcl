INCLUDEDIR = -I../../../shared/nvidia-opencl/include -Iinc -I/usr/local/cuda/include/
LIBDIR = -L../../../shared/nvidia-opencl/lib -L/usr/local/cuda/lib64/
CFLAGS=-Wall -m64 -O3 -DUNIX
LIB= -lshrUtils -loclUtils -lOpenCL

build: # obj/oclScan.cpp.o obj/matrixMul_gold.cpp.o
	g++ $(CFLAGS) $(INCLUDEDIR) $(LIBDIR) -o oclScan src/main.cpp src/oclScan_launcher.cpp src/oclScan_gold.cpp $(LIB)

obj:
	mkdir -p obj

# obj/matrixMul_gold.cpp.o: src/matrixMul_gold.cpp obj
# 	g++ $(CFLAGS) $(INCLUDEDIR) -o obj/matrixMul_gold.cpp.o -c src/matrixMul_gold.cpp

# obj/oclScan.cpp.o: src/oclScan.cpp obj
# 	g++ $(CFLAGS) $(INCLUDEDIR) -o obj/oclScan.cpp.o -c src/oclScan.cpp src/matrixMul_gold.cpp


clean:
	rm -rf obj
