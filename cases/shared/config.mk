INCLUDE = -I../../../microcl/ -I/usr/local/cuda/include/
LIBDIR = -L../../../microcl/
CFLAGS=-Wall -O2 -std=c99 -D_DEBUG
LIB=-lmcl -lOpenCL

