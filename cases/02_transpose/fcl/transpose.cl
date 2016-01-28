#define _WARPSIZE 32
__kernel void transposeChunked(__local uchar* sbase, int input_0, int input_1, int input_2, __global int* arrInput_3, int lenInput_4, __global int* arrOutput_9, __global int* lenOutput_10) {
    int tileSize_5 = input_0 * input_0;
    int n_6 = input_1 / input_0;
    int m_7 = input_2 / input_0;
    int tileSize_8 = input_0 * input_0;
    int ub_12 = (input_1 / input_0) * (input_2 / input_0);
    for (int id34 = 0; id34 < (ub_12 / get_num_groups(0)); id34++) {
        int i_11 = (get_group_id(0) * (ub_12 / get_num_groups(0))) + id34;
        int i_13 = i_11 / (input_1 / input_0);
        int j_14 = i_11 % (input_1 / input_0);
        int p_15 = ((j_14 * (input_1 / input_0)) + i_13) / n_6;
        int q_16 = ((j_14 * (input_1 / input_0)) + i_13) % n_6;
        int* arr_17 = (int*) (sbase + 0);
        int ub_19 = input_0 * input_0;
        // ForAll
        for (int id32 = 0; id32 < (ub_19 / get_local_size(0)); id32++) {
            int i_18 = (id32 * get_local_size(0)) + get_local_id(0);
            int i_20 = i_18 / input_0;
            int j_21 = i_18 % input_0;
            int i_22 = ((j_21 * input_0) + i_20) / input_0;
            int j_23 = ((j_21 * input_0) + i_20) % input_0;
            arr_17[i_18] = arrInput_3 [((p_15 * (m_7 * tileSize_5)) + ((q_16 * input_0) + ((m_7 * (input_0 * i_22)) + j_23)))];
        }
        if (get_local_id(0) < (ub_19 % get_local_size(0))) {
            int i_18 = ((ub_19 / get_local_size(0)) * get_local_size(0)) + get_local_id(0);
            int i_20 = i_18 / input_0;
            int j_21 = i_18 % input_0;
            int i_22 = ((j_21 * input_0) + i_20) / input_0;
            int j_23 = ((j_21 * input_0) + i_20) % input_0;
            arr_17[i_18] = arrInput_3 [((p_15 * (m_7 * tileSize_5)) + ((q_16 * input_0) + ((m_7 * (input_0 * i_22)) + j_23)))];
        }
        barrier(CLK_LOCAL_MEM_FENCE);
        int ub_25 = input_0 * input_0;
        // ForAll
        for (int id33 = 0; id33 < (ub_25 / get_local_size(0)); id33++) {
            int i_24 = (id33 * get_local_size(0)) + get_local_id(0);
            int p_26 = i_11;
            int q_27 = i_24;
            int outerRow_28 = p_26 / (input_2 / input_0);
            int outerCol_29 = p_26 % (input_2 / input_0);
            int innerRow_30 = q_27 % input_0;
            int innerCol_31 = q_27 % input_0;
            arrOutput_9[((outerRow_28 * ((input_2 / input_0) * tileSize_8)) + ((innerRow_30 * ((input_2 / input_0) * input_0)) + ((outerCol_29 * input_0) + innerCol_31)))] = arr_17 [i_24];
        }
        if (get_local_id(0) < (ub_25 % get_local_size(0))) {
            int i_24 = ((ub_25 / get_local_size(0)) * get_local_size(0)) + get_local_id(0);
            int p_26 = i_11;
            int q_27 = i_24;
            int outerRow_28 = p_26 / (input_2 / input_0);
            int outerCol_29 = p_26 % (input_2 / input_0);
            int innerRow_30 = q_27 % input_0;
            int innerCol_31 = q_27 % input_0;
            arrOutput_9[((outerRow_28 * ((input_2 / input_0) * tileSize_8)) + ((innerRow_30 * ((input_2 / input_0) * input_0)) + ((outerCol_29 * input_0) + innerCol_31)))] = arr_17 [i_24];
        }
        barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (get_group_id(0) < (ub_12 % get_num_groups(0))) {
        int i_11 = (get_num_groups(0) * (ub_12 / get_num_groups(0))) + get_group_id(0);
        int i_13 = i_11 / (input_1 / input_0);
        int j_14 = i_11 % (input_1 / input_0);
        int p_15 = ((j_14 * (input_1 / input_0)) + i_13) / n_6;
        int q_16 = ((j_14 * (input_1 / input_0)) + i_13) % n_6;
        int* arr_17 = (int*) (sbase + 0);
        int ub_19 = input_0 * input_0;
        // ForAll
        for (int id32 = 0; id32 < (ub_19 / get_local_size(0)); id32++) {
            int i_18 = (id32 * get_local_size(0)) + get_local_id(0);
            int i_20 = i_18 / input_0;
            int j_21 = i_18 % input_0;
            int i_22 = ((j_21 * input_0) + i_20) / input_0;
            int j_23 = ((j_21 * input_0) + i_20) % input_0;
            arr_17[i_18] = arrInput_3 [((p_15 * (m_7 * tileSize_5)) + ((q_16 * input_0) + ((m_7 * (input_0 * i_22)) + j_23)))];
        }
        if (get_local_id(0) < (ub_19 % get_local_size(0))) {
            int i_18 = ((ub_19 / get_local_size(0)) * get_local_size(0)) + get_local_id(0);
            int i_20 = i_18 / input_0;
            int j_21 = i_18 % input_0;
            int i_22 = ((j_21 * input_0) + i_20) / input_0;
            int j_23 = ((j_21 * input_0) + i_20) % input_0;
            arr_17[i_18] = arrInput_3 [((p_15 * (m_7 * tileSize_5)) + ((q_16 * input_0) + ((m_7 * (input_0 * i_22)) + j_23)))];
        }
        barrier(CLK_LOCAL_MEM_FENCE);
        int ub_25 = input_0 * input_0;
        // ForAll
        for (int id33 = 0; id33 < (ub_25 / get_local_size(0)); id33++) {
            int i_24 = (id33 * get_local_size(0)) + get_local_id(0);
            int p_26 = i_11;
            int q_27 = i_24;
            int outerRow_28 = p_26 / (input_2 / input_0);
            int outerCol_29 = p_26 % (input_2 / input_0);
            int innerRow_30 = q_27 % input_0;
            int innerCol_31 = q_27 % input_0;
            arrOutput_9[((outerRow_28 * ((input_2 / input_0) * tileSize_8)) + ((innerRow_30 * ((input_2 / input_0) * input_0)) + ((outerCol_29 * input_0) + innerCol_31)))] = arr_17 [i_24];
        }
        if (get_local_id(0) < (ub_25 % get_local_size(0))) {
            int i_24 = ((ub_25 / get_local_size(0)) * get_local_size(0)) + get_local_id(0);
            int p_26 = i_11;
            int q_27 = i_24;
            int outerRow_28 = p_26 / (input_2 / input_0);
            int outerCol_29 = p_26 % (input_2 / input_0);
            int innerRow_30 = q_27 % input_0;
            int innerCol_31 = q_27 % input_0;
            arrOutput_9[((outerRow_28 * ((input_2 / input_0) * tileSize_8)) + ((innerRow_30 * ((input_2 / input_0) * input_0)) + ((outerCol_29 * input_0) + innerCol_31)))] = arr_17 [i_24];
        }
        barrier(CLK_LOCAL_MEM_FENCE);
    }
    lenOutput_10[0] = ((input_1 / input_0) * (input_2 / input_0)) * tileSize_8;
}
