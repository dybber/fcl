INCLUDEDIR = -I../../shared/nvidia-opencl/include -Isrc
LIBDIR = -L../../shared/nvidia-opencl/lib
CFLAGS=-Wall -m64 -O3 -DUNIX
LIB= -lshrUtils -loclUtils -lOpenCL

build: obj/oclReduction.cpp.o # obj/reduction_gold.cpp.o
	g++ $(CFLAGS) $(LIBDIR) -o oclReduction obj/oclReduction.cpp.o $(LIB)

obj:
	mkdir -p obj

# obj/reduction_gold.cpp.o: src/reduction_gold.cpp obj
# 	g++ $(CFLAGS) $(INCLUDEDIR) -o obj/reduction_gold.cpp.o -c src/reduction_gold.cpp

obj/oclReduction.cpp.o: src/oclReduction.cpp obj
	g++ $(CFLAGS) $(INCLUDEDIR) -o obj/oclReduction.cpp.o -c src/oclReduction.cpp


clean:
	rm -rf obj
