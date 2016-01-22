// Adapted from
// http://www.drdobbs.com/parallel/cuda-supercomputing-for-the-masses-part/208801731?pgno=2

#define BLOCK_SIZE 256

__kernel void staticReverse(__global int *d_out, __global int *d_in)
{
    __local int s_data[BLOCK_SIZE];

    int inOffset  = BLOCK_SIZE * get_group_id(0);
    int in  = inOffset + get_local_id(0);
 
    // Load one element per thread from device memory and store it 
    // *in reversed order* into temporary shared memory
    s_data[BLOCK_SIZE - 1 - get_local_id(0)] = d_in[in];
 
    // Block until all threads in the block have written their data to shared mem
    barrier(CLK_LOCAL_MEM_FENCE);
 
    // write the data from shared memory in forward order, 
    // but to the reversed block offset as before
 
    int outOffset = BLOCK_SIZE * (get_num_groups(0) - 1 - get_group_id(0));
 
    int out = outOffset + get_local_id(0);
    d_out[out] = s_data[get_local_id(0)];
}

__kernel void dynamicReverse(__global int *d_out, __global int *d_in,
                             __local int *s_data)
{
    int inOffset  = get_local_size(0) * get_group_id(0);
    int in  = inOffset + get_local_id(0);
 
    // Load one element per thread from device memory and store it 
    // *in reversed order* into temporary shared memory
    s_data[get_local_size(0) - 1 - get_local_id(0)] = d_in[in];
 
    // Block until all threads in the block have written their data to shared mem
    barrier(CLK_LOCAL_MEM_FENCE);
 
    // write the data from shared memory in forward order, 
    // but to the reversed block offset as before
 
    int outOffset = get_local_size(0) * (get_num_groups(0) - 1 - get_group_id(0));
 
    int out = outOffset + get_local_id(0);
    d_out[out] = s_data[get_local_id(0)];
}
