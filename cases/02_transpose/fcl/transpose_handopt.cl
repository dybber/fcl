#define _WARPSIZE 32
#define BLOCK_DIM 16
#define LOG_BLOCK_DIM 4

#define OUTER_DIM 128
#define LOG_OUTER_DIM 7

// make local_size statically known + dead code removal - okay effect
// make get_num_groups statically known + dead code removal - small effect
// convert divisions, modulus to uses of >> and &   -- large effect!
// improve peep-hole optimizer wrt. multiplication
// common sub-expression elimination

__kernel void transposeChunked2048x2048(__local uchar* sbase, __global int* arrInput_0, int lenInput_1, __global int* arrOutput_5) {
    __local int* arr_12 = (__local int*) (sbase + 0);
    for (int id31 = 0; id31 < 64; id31++) {
      int i_6 = (get_group_id(0) * 64) + id31; // virtual group id
      int i_13 = get_local_id(0);
      int outerRow_23 = i_6 >> LOG_OUTER_DIM;
      int outerCol_24 = i_6 & (OUTER_DIM-1);
      int innerRow_25 = i_13 >> LOG_BLOCK_DIM;
      int innerCol_26 = i_13 & (BLOCK_DIM-1);

      int p_10 = ((outerCol_24 * 128) + outerRow_23) >> LOG_OUTER_DIM;
      int q_11 = ((outerCol_24 * 128) + outerRow_23) & (OUTER_DIM-1);

      int i_17 = ((innerCol_26 * 16) + innerRow_25) >> LOG_BLOCK_DIM;
      int j_18 = ((innerCol_26 * 16) + innerRow_25) & (BLOCK_DIM-1);
      arr_12[i_13] = arrInput_0 [p_10 * 32768
                                 + q_11 * 16
                                 + i_17 * 2048
                                 + j_18];

      barrier(CLK_LOCAL_MEM_FENCE);

      arrOutput_5[outerRow_23 * 32768
                  + innerRow_25 * 2048
                  + outerCol_24 * 16
                  + innerCol_26] = arr_12 [i_13];

      barrier(CLK_LOCAL_MEM_FENCE);
    }
}
