INCLUDE = -I../../../microcl/ -I../../../include/ -I/usr/local/cuda/include/
LIBDIR = -L../../../microcl/ -L/usr/local/cuda/lib64/
CFLAGS=-Wall -Wno-unused-variable -O2 -D_DEBUG -std=c99
LIB=-lmcl -lOpenCL
