
INCLUDE=-Iinclude
CFLAGS= -Wall -m64 -O2 -std=c++11

build: lib/libclu_runtime.a

obj/clu_runtime.cpp.o: src/clu_runtime.cpp
	mkdir -p obj
	g++ $(CFLAGS) $(INCLUDE) -o $@ -c $^

lib/libclu_runtime.a: obj/clu_runtime.cpp.o
	mkdir -p lib
	ar rucv $@ $^

clean:
	rm -rf lib
	rm -rf obj
