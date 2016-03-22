//#pragma OPENCL EXTENSION cl_amd_printf : enable

#define BLOCK_DIM 16
#define LOG_BLOCK_DIM 4

// From NVIDIA


/*
 * Copyright 1993-2010 NVIDIA Corporation.  All rights reserved.
 *
 * Please refer to the NVIDIA end user license agreement (EULA) associated
 * with this source code for terms and conditions that govern your use of
 * this software. Any use, reproduction, disclosure, or distribution of
 * this software and related documentation outside the terms of the EULA
 * is strictly prohibited.
 *
 */

__kernel void transpose(__global int *odata, __global int *idata, int offset, int width, int height, __local int* block)
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


// Modified version of above, to run over a 1D-grid, calculating the
// 2D coordinates instead
__kernel void transpose1D(__global int *odata, __global int *idata, int offset, int width, int height, __local int* block)
{
  unsigned int groupsPerRow = width >> LOG_BLOCK_DIM; // width / BLOCK_DIM
  unsigned const int groupIDx = get_group_id(0) & (groupsPerRow-1); // get_group_id(0) % groupsPerRow
  unsigned const int groupIDy = get_group_id(0) >> 7; // get_group_id(0) / groupsPerRow

  unsigned const int localIDx = get_local_id(0) & (BLOCK_DIM-1); // get_local_id(0) % BLOCK_DIM
  unsigned const int localIDy = get_local_id(0) >> LOG_BLOCK_DIM; // get_local_id(0) / BLOCK_DIM

	// read the matrix tile into shared memory
	unsigned int xIndex = groupIDx * BLOCK_DIM + localIDx;
	unsigned int yIndex = groupIDy * BLOCK_DIM + localIDy;

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
