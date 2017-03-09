INCLUDE = -I../../../microcl/ -I../../../include/ -I/usr/local/cuda/include/
LIBDIR = -L../../../microcl/
CFLAGS=-Wall -O2 -D_DEBUG -std=c99
LIB=-lmcl -lOpenCL

