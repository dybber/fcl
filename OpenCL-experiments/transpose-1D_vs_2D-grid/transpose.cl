__kernel void transpose(__global float *odata, __global float *idata, int offset, int width, int height, __local float* block)
{
	// read the matrix tile into shared memory
	unsigned int xIndex = get_global_id(0);
	unsigned int yIndex = get_global_id(1);

	if((xIndex + offset < width) && (yIndex < height))
	{
		unsigned int index_in = yIndex * width + xIndex + offset;
		block[get_local_id(1)*(BLOCK_DIM+1)+get_local_id(0)] = idata[index_in];
	}

	barrier(CLK_LOCAL_MEM_FENCE);

	// write the transposed matrix tile to global memory
	xIndex = get_group_id(1) * BLOCK_DIM + get_local_id(0);
	yIndex = get_group_id(0) * BLOCK_DIM + get_local_id(1);
	if((xIndex < height) && (yIndex + offset < width))
    {
		unsigned int index_out = yIndex * height + xIndex;
		odata[index_out] = block[get_local_id(0)*(BLOCK_DIM+1)+get_local_id(1)];
	}
}


__kernel void transpose1D(__global float *odata, __global float *idata, int offset, int width, int height, __local float* block)
{
	// read the matrix tile into shared memory
	unsigned int xIndex = get_global_id(0) % width;
	unsigned int yIndex = get_global_id(0) / width;
  unsigned int localIDx = xIndex % BLOCK_DIM;
  unsigned int localIDy = yIndex % BLOCK_DIM;
  unsigned int groupIDx = xIndex / BLOCK_DIM;
  unsigned int groupIDy = yIndex / BLOCK_DIM;
    

	if((xIndex + offset < width) && (yIndex < height))
	{
		unsigned int index_in = yIndex * width + xIndex + offset;
		block[localIDy*(BLOCK_DIM+1)+localIDx] = idata[index_in];
	}

	barrier(CLK_LOCAL_MEM_FENCE);

	// write the transposed matrix tile to global memory
	xIndex = groupIDy * BLOCK_DIM + localIDx;
	yIndex = groupIDx * BLOCK_DIM + localIDy;
	if((xIndex < height) && (yIndex + offset < width))
    {
		unsigned int index_out = yIndex * height + xIndex;
		odata[index_out] = block[localIDx*(BLOCK_DIM+1)+localIDy];
	}
}
