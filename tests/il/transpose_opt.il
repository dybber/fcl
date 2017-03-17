arr_0 = allocate(int,16777216);
distribute<block> (i_1 < 65536) {
    arr_3 = allocate(int,256);
    int x = ((((i_1 & 255) * 256) + (i_1 >> 8)) & 255);
    int y = (x * 16);
    parfor<block> (i_4 < 256) {
        arr_3[i_4] = (((y + (i_4 >> 4)) * 4096) + (((x >> 8) * 16) + (i_4 & 15)));
    }
    int c = ((i_1 & 255) * 16);
    int b = ((i_1 >> 8) * 16);
    parfor<block> (i_6 < 256) {
        arr_0[(((c + (i_6 >> 4)) * 4096) + (b + (i_6 & 15)))] = arr_3[(((i_6 & 15) * 16) + (i_6 >> 4))];
    }
}
printIntArray(100, arr_0);
benchmark(100) {
    distribute<block> (i_1 < 65536) {
        arr_3 = allocate(int,256);
        int x = ((((i_1 & 255) * 256) + (i_1 >> 8)) & 255);
        int y = (x * 16);
        parfor<block> (i_4 < 256) {
            arr_3[i_4] = (((y + (i_4 >> 4)) * 4096) + (((x >> 8) * 16) + (i_4 & 15)));
        }
        int c = ((i_1 & 255) * 16);
        int b = ((i_1 >> 8) * 16);
        parfor<block> (i_6 < 256) {
            arr_0[(((c + (i_6 >> 4)) * 4096) + (b + (i_6 & 15)))] = arr_3[(((i_6 & 15) * 16) + (i_6 >> 4))];
        }
    }
}
