__kernel void simple_copy1D(__global float *odata, __global float* idata, int size)
{
    unsigned int i = get_global_id(0);
    
    if (i < size) {
        odata[i] = idata[i]; 
    }
}

__kernel void shared_copy1D(__global float *odata, __global float *idata, int size, __local float* block) {
	// read the matrix tile into shared memory
	unsigned int i = get_global_id(0);

	if(i < size) {
		block[get_local_id(0)] = idata[i];
	}

	barrier(CLK_LOCAL_MEM_FENCE);

	if(i < size) {
		odata[i] = block[get_local_id(0)];
	}
}

#define BLOCK_SIZE 256
#define NUM_GROUPS 4096

__kernel void simple_copy1D_virt(__global float *odata, __global float* idata, int size) {
  int elems_per_wg = size / NUM_GROUPS;
  int groups_per_wg = elems_per_wg / BLOCK_SIZE;
  int offset = elems_per_wg * get_group_id(0);
  for (int gid = 0; gid < groups_per_wg; gid++) {
    int i = offset + (gid * BLOCK_SIZE) + get_local_id(0);
    odata[i] = idata[i];
  }
}

__kernel void simple_copy1D_virt_strength_reduced(__global float *odata, __global float* idata, int size) {
  int elems_per_wg = size / NUM_GROUPS;
  int groups_per_wg = elems_per_wg / BLOCK_SIZE;
  int offset = elems_per_wg * get_group_id(0);
  int max = offset + (groups_per_wg * BLOCK_SIZE);
  for (int i = offset + get_local_id(0); i < max; i += BLOCK_SIZE) {
    odata[i] = idata[i];
  }
}

