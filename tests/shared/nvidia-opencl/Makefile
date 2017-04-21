INCLUDE=-Iinclude -I/usr/local/cuda/include/ -L/usr/local/cuda/lib64
CFLAGS= -Wall -m64 -O2 -DUNIX

build: lib/libshrutil.a lib/liboclUtil.a

# shrUtils
obj/shrUtils.cpp.o: src/shrUtils.cpp
	mkdir -p obj
	g++ $(CFLAGS) $(INCLUDE) -o $@ -c src/shrUtils.cpp

obj/cmd_arg_reader.cpp.o: src/cmd_arg_reader.cpp
	mkdir -p obj
	g++ -Wall -m64  $(INCLUDE) -O2 -o $@ -c src/cmd_arg_reader.cpp

lib/libshrutil.a: obj/shrUtils.cpp.o obj/cmd_arg_reader.cpp.o
	mkdir -p lib
	ar rucv lib/libshrUtils.a obj/shrUtils.cpp.o obj/cmd_arg_reader.cpp.o

# oclUtils
obj/oclUtils.cpp.o:
	mkdir -p obj
	g++ -Wall -m64 $(INCLUDE) -O3 -o obj/oclUtils.cpp.o -c src/oclUtils.cpp

lib/liboclUtil.a: obj/oclUtils.cpp.o
	mkdir -p lib
	ar qv lib/liboclUtils.a obj/oclUtils.cpp.o  

clean:
	rm -rf lib
	rm -rf obj
