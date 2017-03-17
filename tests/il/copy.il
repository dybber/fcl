arr_0 = allocate(int,muli(256,divi(muli(4096,4096),256)));
int ub_2 = divi(muli(4096,4096),256);
distribute<block> (i_1< ub_2) {
    int ub_4 = 256;
    parfor<block> (i_3< ub_4) {
        arr_0[addi(muli(i_1,256),i_3)] = addi(muli(i_1,256),i_3);
    }
}
printIntArray(100, arr_0);
// benchmark(100) {
//     arr_5 = allocate(int,muli(256,divi(muli(4096,4096),256)));
//     int ub_7 = divi(muli(4096,4096),256);
//     distribute<block> (i_6< ub_7) {
//         int ub_9 = 256;
//         parfor<block> (i_8< ub_9) {
//             arr_5[addi(muli(i_6,256),i_8)] = addi(muli(i_6,256),i_8);
//         }
//     }
// }
