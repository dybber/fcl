#define _WARPSIZE 32
__kernel void reverseGrid512(__local uchar* sbase, __global int* arrInput_0, int lenInput_1, __global int* arrOutput_3, __global int* lenOutput_4) {
    int n_2 = ((lenInput_1 + 512) - 1) / 512;
    int ub_6 = n_2;
    for (int id15 = 0; id15 < (ub_6 / get_num_groups(0)); id15++) {
        int i_5 = (get_group_id(0) * (ub_6 / get_num_groups(0))) + id15;
        int* arr_8 = (int*) (sbase + 0);
        int ub_10 = 512;
        // ForAll
        for (int id13 = 0; id13 < (ub_10 / get_local_size(0)); id13++) {
            int i_9 = (id13 * get_local_size(0)) + get_local_id(0);
            arr_8[i_9] = arrInput_0 [((((n_2 - 1) - i_5) * 512) + ((512 - i_9) - 1))];
        }
        if (get_local_id(0) < (512 % get_local_size(0))) {
            int i_9 = ((512 / get_local_size(0)) * get_local_size(0)) + get_local_id(0);
            arr_8[i_9] = arrInput_0 [((((n_2 - 1) - i_5) * 512) + ((512 - i_9) - 1))];
        }
        barrier(CLK_LOCAL_MEM_FENCE);
        int ub_12 = 512;
        // ForAll
        for (int id14 = 0; id14 < (ub_12 / get_local_size(0)); id14++) {
            int i_11 = (id14 * get_local_size(0)) + get_local_id(0);
            arrOutput_3[((i_5 * 512) + i_11)] = arr_8 [i_11];
        }
        if (get_local_id(0) < (512 % get_local_size(0))) {
            int i_11 = ((512 / get_local_size(0)) * get_local_size(0)) + get_local_id(0);
            arrOutput_3[((i_5 * 512) + i_11)] = arr_8 [i_11];
        }
        barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (get_group_id(0) < (ub_6 % get_num_groups(0))) {
        int i_5 = (get_num_groups(0) * (ub_6 / get_num_groups(0))) + get_group_id(0);
        int* arr_8 = (int*) (sbase + 0);
        int ub_10 = 512;
        // ForAll
        for (int id13 = 0; id13 < (ub_10 / get_local_size(0)); id13++) {
            int i_9 = (id13 * get_local_size(0)) + get_local_id(0);
            arr_8[i_9] = arrInput_0 [((((n_2 - 1) - i_5) * 512) + ((512 - i_9) - 1))];
        }
        if (get_local_id(0) < (512 % get_local_size(0))) {
            int i_9 = ((512 / get_local_size(0)) * get_local_size(0)) + get_local_id(0);
            arr_8[i_9] = arrInput_0 [((((n_2 - 1) - i_5) * 512) + ((512 - i_9) - 1))];
        }
        barrier(CLK_LOCAL_MEM_FENCE);
        int ub_12 = 512;
        // ForAll
        for (int id14 = 0; id14 < (ub_12 / get_local_size(0)); id14++) {
            int i_11 = (id14 * get_local_size(0)) + get_local_id(0);
            arrOutput_3[((i_5 * 512) + i_11)] = arr_8 [i_11];
        }
        if (get_local_id(0) < (512 % get_local_size(0))) {
            int i_11 = ((512 / get_local_size(0)) * get_local_size(0)) + get_local_id(0);
            arrOutput_3[((i_5 * 512) + i_11)] = arr_8 [i_11];
        }
        barrier(CLK_LOCAL_MEM_FENCE);
    }
    lenOutput_4[0] = n_2 * 512;
}
