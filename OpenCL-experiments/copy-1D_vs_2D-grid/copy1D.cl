__kernel void simple_copy1D(__global float *odata, __global float* idata, int size)
{
    unsigned int i = get_global_id(0);
    
    if (i < size) {
        odata[i] = idata[i]; 
    }
}

#define BLOCK_SIZE 256

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
