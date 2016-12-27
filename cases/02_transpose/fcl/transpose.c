#include <stdio.h>
#include <sys/time.h>
#include <mcl.h>
#include <fcl.h>
int main() {
    mclContext ctx_0 = mclInitialize(2);
    cl_program program_1 = mclBuildProgram(ctx_0, "transpose.cl");
    mclDeviceData buffer_2 = mclAllocDevice(ctx_0, MCL_RW, sizeof(int), (((12 / 4) * (12 / 4)) * 16));
    int ub_2_3 = ((12 / 4) * (12 / 4));
    cl_kernel kernel_7 = mclCreateKernel(program_1, "kernel_4");
    mclSetKernelArg(kernel_7, 0, 2048, NULL);
    mclSetKernelArg(kernel_7, 1, sizeof(cl_mem), &(buffer_2.data));
    int arg_56 = ub_2_3;
    mclSetKernelArg(kernel_7, 2, sizeof(int), &(arg_56));
    mclInvokeKernel(ctx_0, kernel_7, (256 * 2048), 256);
    mclFinish(ctx_0);
    void* hostMMap_57 = mclMap(ctx_0, buffer_2, CL_MAP_READ, (12 * sizeof(int)));
    int* cast_58 = ((int*) hostMMap_57);
    int ub_60 = 12;
    for (int i_59 = 0; i_59 < ub_60; i_59++) {
        printf("%i: %i\n", i_59, cast_58 [i_59]);
    }
    mclUnmap(ctx_0, buffer_2, cast_58);
    mclReleaseDeviceData(&(buffer_2));
    mclReleaseKernel(kernel_7);
    mclReleaseProgram(program_1);
    mclReleaseContext(&(ctx_0));
}