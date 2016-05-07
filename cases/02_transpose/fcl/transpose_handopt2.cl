#define _WARPSIZE 32
#define BLOCK_DIM 16
#define LOG_BLOCK_DIM 4
#define OUTER_DIM 128
#define LOG_OUTER_DIM 7

__kernel void transposeChunked16(__local uchar* sbase, int input_0, int input_1,
                                 __global int* arrInput_2, int lenInput_3, __global int* arrOutput_7) {
    __local int* arr_14 = (__local int*) (sbase + 0);

    int n_5 = input_0 >> LOG_BLOCK_DIM;
    int m_6 = input_1 >> LOG_BLOCK_DIM;
    int ub_9 = n_5 * m_6;
    int id34 = ub_9 / get_num_groups(0);

    int innerRow_27 = get_local_id(0) >> LOG_BLOCK_DIM;
    int innerCol_28 = get_local_id(0) & (BLOCK_DIM-1);

    for (int id33 = 0; id33 < id34; id33++) {
        int i_8 = (get_group_id(0) * id34) + id33;
        int outerRow_25 = i_8 >> LOG_OUTER_DIM;
        int outerCol_26 = i_8 & (OUTER_DIM-1);

        int p_12 = ((outerCol_26 * n_5) + outerRow_25) >> LOG_OUTER_DIM;
        int q_13 = ((outerCol_26 * n_5) + outerRow_25) & (OUTER_DIM-1);

        arr_14[get_local_id(0)] =
          arrInput_2 [((p_12 * (m_6 * 256)) + ((q_13 * 16) + ((m_6 * (16 * innerRow_27)) + innerCol_28)))];

        barrier(CLK_LOCAL_MEM_FENCE);


        arrOutput_7[(((outerRow_25 * m_6) * 256) + (((innerRow_27 * m_6) * 16) + ((outerCol_26 * 16) + innerCol_28)))] =
          arr_14 [((innerCol_28 * 16) + innerRow_27)];
        barrier(CLK_LOCAL_MEM_FENCE);
    }
}
