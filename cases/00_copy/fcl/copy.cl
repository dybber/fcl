#define _WARPSIZE 32
__kernel void copy(__global int* arrInput_0, int lenInput_1, __global int* arrOutput_2, __global int* lenOutput_3) {
    int ub_5 = ((lenInput_1 + 16) - 1) / 16;
    for (int id9 = 0; id9 < (ub_5 / get_num_groups(0)); id9++) {
        int i_4 = (get_group_id(0) * (ub_5 / get_num_groups(0))) + id9;
        int ub_7 = 16;
        // ForAll
        for (int id8 = 0; id8 < (ub_7 / get_local_size(0)); id8++) {
            int i_6 = (id8 * get_local_size(0)) + get_local_id(0);
            arrOutput_2[((i_4 * 16) + i_6)] = arrInput_0 [((i_4 * 16) + i_6)];
        }
        if (get_local_id(0) < (16 % get_local_size(0))) {
            int i_6 = ((16 / get_local_size(0)) * get_local_size(0)) + get_local_id(0);
            arrOutput_2[((i_4 * 16) + i_6)] = arrInput_0 [((i_4 * 16) + i_6)];
        }
        barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (get_group_id(0) < (ub_5 % get_num_groups(0))) {
        int i_4 = (get_num_groups(0) * (ub_5 / get_num_groups(0))) + get_group_id(0);
        int ub_7 = 16;
        // ForAll
        for (int id8 = 0; id8 < (ub_7 / get_local_size(0)); id8++) {
            int i_6 = (id8 * get_local_size(0)) + get_local_id(0);
            arrOutput_2[((i_4 * 16) + i_6)] = arrInput_0 [((i_4 * 16) + i_6)];
        }
        if (get_local_id(0) < (16 % get_local_size(0))) {
            int i_6 = ((16 / get_local_size(0)) * get_local_size(0)) + get_local_id(0);
            arrOutput_2[((i_4 * 16) + i_6)] = arrInput_0 [((i_4 * 16) + i_6)];
        }
        barrier(CLK_LOCAL_MEM_FENCE);
    }
    lenOutput_3[0] = (lenInput_1 + 16) - 1;
}
