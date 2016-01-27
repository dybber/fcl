/* Unmodified code from NVIDIA SDK */

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

__kernel void simple_copy(__global float *odata, __global float* idata, int offset, int width, int height)
{
    unsigned int xIndex = get_global_id(0);
    unsigned int yIndex = get_global_id(1);
    
    if (xIndex + offset < width && yIndex < height)
    {
        unsigned int index_in  = xIndex + offset + width * yIndex;
        odata[index_in] = idata[index_in]; 
    }
}

#define BLOCK_DIM 16

__kernel void shared_copy(__global float *odata, __global float *idata, int offset, int width, int height, __local float* block)
{
	// read the matrix tile into shared memory
	unsigned int xIndex = get_global_id(0);
	unsigned int yIndex = get_global_id(1);

    unsigned int index_in = yIndex * width + xIndex + offset;
	if((xIndex + offset< width) && (yIndex < height))
	{
		block[get_local_id(1)*(BLOCK_DIM+1)+get_local_id(0)] = idata[index_in];
	}

	barrier(CLK_LOCAL_MEM_FENCE);

	if((xIndex < height) && (yIndex+ offset < width))
    {
		odata[index_in] = block[get_local_id(1)*(BLOCK_DIM+1)+get_local_id(0)];
	}
}

__kernel void uncoalesced_copy(__global float *odata, __global float* idata, int offset, int width, int height)
{
    unsigned int xIndex = get_global_id(0);
    unsigned int yIndex = get_global_id(1);
    
    if (xIndex + offset < width && yIndex < height)
    {
        unsigned int index_in  = yIndex + height * (xIndex+ offset);
        odata[index_in] = idata[index_in]; 
    }
}
