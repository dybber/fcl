#define _WARPSIZE 32
#define BLOCK_DIM 16
#define LOG_BLOCK_DIM 4

// common subexpression elimination
// copy propagation
//   -> leading to extra dead code eliminationo
// loop hoisting
// optimized division to use shifts
__kernel void transposeChunked16(__local uchar* sbase, int input_0, int input_1, __global int* arrInput_2, int lenInput_3, __global int* arrOutput_7) {
  __local int* arr_14 = (__local int*) (sbase + 0);
    int n_5 = input_0 >> LOG_BLOCK_DIM;
    int groupsPerRow_6 = input_1 >> LOG_BLOCK_DIM;
    int ub_9 = n_5 * groupsPerRow_6;
    int id40 = ub_9 / get_num_groups(0);
    int i_15 = get_local_id(0);
    int localIDy_17 = i_15 >> LOG_BLOCK_DIM;
    int localIDx_18 = i_15 & (BLOCK_DIM-1);

    for (int id39 = 0; id39 < id40; id39++) {
        int i_8 = (get_group_id(0) * id40) + id39;
        /* int i_10 = i_8 / n_5; */
        /* int j_11 = i_8 % n_5; */
        /* int groupIDy_12 = ((j_11 * n_5) + i_10) / groupsPerRow_6; */
        /* int groupIDx_13 = ((j_11 * n_5) + i_10) % groupsPerRow_6; */

        int groupIDx_28 = i_8 % groupsPerRow_6;
        int groupIDy_29 = i_8 / groupsPerRow_6;

        int xIndex_20 = (groupIDx_28 * 16) + localIDx_18;
        int yIndex_19 = (groupIDy_29 * 16) + localIDy_17;

        int ix_21 = (yIndex_19 * input_1) + xIndex_20;
        
        arr_14[localIDy_17*(BLOCK_DIM+1)+localIDx_18] = arrInput_2 [ix_21];
        barrier(CLK_LOCAL_MEM_FENCE);

        int xIndex_32 = (groupIDy_29 * 16) + localIDx_18;
        int yIndex_33 = (groupIDx_28 * 16) + localIDy_17;
        int ix_34 = (yIndex_33 * input_0) + xIndex_32;
        arrOutput_7[ix_34] = arr_14 [((localIDx_18 * (BLOCK_DIM+1)) + localIDy_17)];
        barrier(CLK_LOCAL_MEM_FENCE);
    }
    /* if (get_group_id(0) < (ub_9 % get_num_groups(0))) { */
    /*     int i_8 = (get_num_groups(0) * id40) + get_group_id(0); */
    /*     int i_10 = i_8 / n_5; */
    /*     int j_11 = i_8 % n_5; */
    /*     int groupIDy_12 = ((j_11 * n_5) + i_10) / groupsPerRow_6; */
    /*     int groupIDx_13 = ((j_11 * n_5) + i_10) % groupsPerRow_6; */
    /*     __local int* arr_14 = (__local int*) (sbase + 0); */
    /*     int id36 = 256 / 256; */
    /*     for (int id35 = 0; id35 < id36; id35++) { */
    /*         int i_15 = (id35 * 256) + get_local_id(0); */
    /*         int localIDy_17 = i_15 >> LOG_BLOCK_DIM; */
    /*         int localIDx_18 = i_15 & (BLOCK_DIM-1); */
    /*         int yIndex_19 = (groupIDx_13 * 16) + localIDy_17; */
    /*         int xIndex_20 = (groupIDy_12 * 16) + localIDx_18; */
    /*         int ix_21 = (yIndex_19 * input_1) + xIndex_20; */
    /*         arr_14[i_15] = arrInput_2 [ix_21]; */
    /*     } */
    /*     if (get_local_id(0) < (256 % 256)) { */
    /*         int i_15 = (id36 * 256) + get_local_id(0); */
    /*         int localIDy_17 = i_15 >> LOG_BLOCK_DIM; */
    /*         int localIDx_18 = i_15 & (BLOCK_DIM-1); */
    /*         int yIndex_19 = (groupIDx_13 * 16) + localIDy_17; */
    /*         int xIndex_20 = (groupIDy_12 * 16) + localIDx_18; */
    /*         int ix_21 = (yIndex_19 * input_1) + xIndex_20; */
    /*         arr_14[i_15] = arrInput_2 [ix_21]; */
    /*     } */
    /*     barrier(CLK_LOCAL_MEM_FENCE); */
    /*     int id38 = 256 / 256; */
    /*     for (int id37 = 0; id37 < id38; id37++) { */
    /*         int i_22 = (id37 * 256) + get_local_id(0); */
    /*         int i_24 = i_22 >> LOG_BLOCK_DIM; */
    /*         int j_25 = i_22 & (BLOCK_DIM-1); */
    /*         int gid_26 = i_8; */
    /*         int tid_27 = i_22; */
    /*         int groupIDx_28 = gid_26 % groupsPerRow_6; */
    /*         int groupIDy_29 = gid_26 / groupsPerRow_6; */
    /*         int localIDx_30 = tid_27 & (BLOCK_DIM-1); */
    /*         int localIDy_31 = tid_27 >> LOG_BLOCK_DIM; */
    /*         int xIndex_32 = (groupIDy_29 * 16) + localIDx_30; */
    /*         int yIndex_33 = (groupIDx_28 * 16) + localIDy_31; */
    /*         int ix_34 = (yIndex_33 * input_0) + xIndex_32; */
    /*         arrOutput_7[ix_34] = arr_14 [((j_25 * 16) + i_24)]; */
    /*     } */
    /*     if (get_local_id(0) < (256 % 256)) { */
    /*         int i_22 = (id38 * 256) + get_local_id(0); */
    /*         int i_24 = i_22 >> LOG_BLOCK_DIM; */
    /*         int j_25 = i_22 & (BLOCK_DIM-1); */
    /*         int gid_26 = i_8; */
    /*         int tid_27 = i_22; */
    /*         int groupIDx_28 = gid_26 % groupsPerRow_6; */
    /*         int groupIDy_29 = gid_26 / groupsPerRow_6; */
    /*         int localIDx_30 = tid_27 & (BLOCK_DIM-1); */
    /*         int localIDy_31 = tid_27 >> LOG_BLOCK_DIM; */
    /*         int xIndex_32 = (groupIDy_29 * 16) + localIDx_30; */
    /*         int yIndex_33 = (groupIDx_28 * 16) + localIDy_31; */
    /*         int ix_34 = (yIndex_33 * input_0) + xIndex_32; */
    /*         arrOutput_7[ix_34] = arr_14 [((j_25 * 16) + i_24)]; */
    /*     } */
    /*     barrier(CLK_LOCAL_MEM_FENCE); */
    /* } */
}
