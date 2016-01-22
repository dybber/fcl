
// Direct translation of CUDA version from NVIDIA blogpost
// http://devblogs.nvidia.com/parallelforall/using-shared-memory-cuda-cc/
// https://github.com/parallel-forall/code-samples/blob/master/series/cuda-cpp/shared-memory/shared-memory.cu

// Original Copyright notice from NVIDIA, see LICENSE file
// (compilation errors if inserted in this file)

__kernel void staticReverse(__global int *d, const int n)
{
  __local int s[64];
  int t = get_global_id(0);
  int tr = n-t-1;
  s[t] = d[t];
  barrier(CLK_LOCAL_MEM_FENCE);
  d[t] = s[tr];
}


__kernel void dynamicReverse(__global int *d, __local int *s, const int n)
{
  int t = get_global_id(0);
  int tr = n-t-1;
  s[t] = d[t];
  barrier(CLK_LOCAL_MEM_FENCE);
  d[t] = s[tr];
}
