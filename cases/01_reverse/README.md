This is a pretty basic usage of shared-memory, using only a single
work-group.

Example from: http://devblogs.nvidia.com/parallelforall/using-shared-memory-cuda-cc/
   
Original CUDA version: https://github.com/parallel-forall/code-samples/blob/master/series/cuda-cpp/shared-memory/shared-memory.cu


The example should probably be extended to use multiple work-items and
not working in-place, if we want to keep it. There's a CUDA version
here:
http://www.drdobbs.com/parallel/cuda-supercomputing-for-the-masses-part/207603131?pgno=2
and this is ported to OpenCL here:
http://openclopedia.weebly.com/blog/a-simple-porting-example
