#define _WARPSIZE 32
__kernel void reduceAdd(__local uchar* sbase, __global int* arrInput_0, int lenInput_1, __global int* arrOutput_2, __global int* lenOutput_3) {
    int ub_5 = ((lenInput_1 + 512) - 1) / 512;
    for (int id22 = 0; id22 < (ub_5 / get_num_groups(0)); id22++) {
        int i_4 = (get_group_id(0) * (ub_5 / get_num_groups(0))) + id22;
        int* arr_8 = (int*) (sbase + 0);
        int arraySize_9 = 256;
        int ub_11 = 256;
        // ForAll
        for (int id19 = 0; id19 < (ub_11 / get_local_size(0)); id19++) {
            int i_10 = (id19 * get_local_size(0)) + get_local_id(0);
            arr_8[i_10] = arrInput_0 [((i_4 * 512) + i_10)] + arrInput_0 [((i_4 * 512) + (i_10 + 256))];
        }
        if (get_local_id(0) < (256 % get_local_size(0))) {
            int i_10 = ((256 / get_local_size(0)) * get_local_size(0)) + get_local_id(0);
            arr_8[i_10] = arrInput_0 [((i_4 * 512) + i_10)] + arrInput_0 [((i_4 * 512) + (i_10 + 256))];
        }
        barrier(CLK_LOCAL_MEM_FENCE);
        bool cond_12 = 1 != 256;
        while (!(cond_12)) {
            int len_13 = arraySize_9 / 2;
            int len_14 = arraySize_9 - (arraySize_9 / 2);
            arraySize_9 = (len_13 < len_14) ? len_13 : len_14;
            int ub_16 = arraySize_9;
            // ForAll
            for (int id20 = 0; id20 < (ub_16 / get_local_size(0)); id20++) {
                int i_15 = (id20 * get_local_size(0)) + get_local_id(0);
                arr_8[i_15] = arr_8 [i_15] + arr_8 [(i_15 + (arraySize_9 / 2))];
            }
            if (get_local_id(0) < (ub_16 % get_local_size(0))) {
                int i_15 = ((ub_16 / get_local_size(0)) * get_local_size(0)) + get_local_id(0);
                arr_8[i_15] = arr_8 [i_15] + arr_8 [(i_15 + (arraySize_9 / 2))];
            }
            barrier(CLK_LOCAL_MEM_FENCE);
            cond_12 = 1 != arraySize_9;
        }
        int ub_18 = arraySize_9;
        // ForAll
        for (int id21 = 0; id21 < (ub_18 / get_local_size(0)); id21++) {
            int i_17 = (id21 * get_local_size(0)) + get_local_id(0);
            arrOutput_2[(i_4 + i_17)] = arr_8 [i_17];
        }
        if (get_local_id(0) < (ub_18 % get_local_size(0))) {
            int i_17 = ((ub_18 / get_local_size(0)) * get_local_size(0)) + get_local_id(0);
            arrOutput_2[(i_4 + i_17)] = arr_8 [i_17];
        }
        barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (get_group_id(0) < (ub_5 % get_num_groups(0))) {
        int i_4 = (get_num_groups(0) * (ub_5 / get_num_groups(0))) + get_group_id(0);
        int* arr_8 = (int*) (sbase + 0);
        int arraySize_9 = 256;
        int ub_11 = 256;
        // ForAll
        for (int id19 = 0; id19 < (ub_11 / get_local_size(0)); id19++) {
            int i_10 = (id19 * get_local_size(0)) + get_local_id(0);
            arr_8[i_10] = arrInput_0 [((i_4 * 512) + i_10)] + arrInput_0 [((i_4 * 512) + (i_10 + 256))];
        }
        if (get_local_id(0) < (256 % get_local_size(0))) {
            int i_10 = ((256 / get_local_size(0)) * get_local_size(0)) + get_local_id(0);
            arr_8[i_10] = arrInput_0 [((i_4 * 512) + i_10)] + arrInput_0 [((i_4 * 512) + (i_10 + 256))];
        }
        barrier(CLK_LOCAL_MEM_FENCE);
        bool cond_12 = 1 != 256;
        while (!(cond_12)) {
            int len_13 = arraySize_9 / 2;
            int len_14 = arraySize_9 - (arraySize_9 / 2);
            arraySize_9 = (len_13 < len_14) ? len_13 : len_14;
            int ub_16 = arraySize_9;
            // ForAll
            for (int id20 = 0; id20 < (ub_16 / get_local_size(0)); id20++) {
                int i_15 = (id20 * get_local_size(0)) + get_local_id(0);
                arr_8[i_15] = arr_8 [i_15] + arr_8 [(i_15 + (arraySize_9 / 2))];
            }
            if (get_local_id(0) < (ub_16 % get_local_size(0))) {
                int i_15 = ((ub_16 / get_local_size(0)) * get_local_size(0)) + get_local_id(0);
                arr_8[i_15] = arr_8 [i_15] + arr_8 [(i_15 + (arraySize_9 / 2))];
            }
            barrier(CLK_LOCAL_MEM_FENCE);
            cond_12 = 1 != arraySize_9;
        }
        int ub_18 = arraySize_9;
        // ForAll
        for (int id21 = 0; id21 < (ub_18 / get_local_size(0)); id21++) {
            int i_17 = (id21 * get_local_size(0)) + get_local_id(0);
            arrOutput_2[(i_4 + i_17)] = arr_8 [i_17];
        }
        if (get_local_id(0) < (ub_18 % get_local_size(0))) {
            int i_17 = ((ub_18 / get_local_size(0)) * get_local_size(0)) + get_local_id(0);
            arrOutput_2[(i_4 + i_17)] = arr_8 [i_17];
        }
        barrier(CLK_LOCAL_MEM_FENCE);
    }
    lenOutput_3[0] = ((lenInput_1 + 512) - 1) / 512;
}
