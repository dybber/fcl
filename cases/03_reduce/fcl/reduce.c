#include <stdio.h>
#include <sys/time.h>
#include <mcl.h>
#include <fcl.h>
int main() {
    mclContext ctx_0 = mclInitialize(2);
    cl_program program_1 = mclBuildProgram(ctx_0, "reduce.cl");
    mclDeviceData buffer_2 = mclAllocDevice(ctx_0, MCL_RW, sizeof(int), ((511 / 512) * 1));
    int ub_2_3 = (511 / 512);
    cl_kernel kernel_7 = mclCreateKernel(program_1, "kernel_4");
    mclSetKernelArg(kernel_7, 0, 2048, NULL);
    mclSetKernelArg(kernel_7, 1, sizeof(cl_mem), &(buffer_2.data));
    int arg_82 = ub_2_3;
    mclSetKernelArg(kernel_7, 2, sizeof(int), &(arg_82));
    mclInvokeKernel(ctx_0, kernel_7, (256 * 2048), 256);
    mclFinish(ctx_0);
    void* hostMMap_83 = mclMap(ctx_0, buffer_2, CL_MAP_READ, (1 * sizeof(int)));
    int* cast_84 = ((int*) hostMMap_83);
    int ub_86 = 1;
    for (int i_85 = 0; i_85 < ub_86; i_85++) {
        printf("%i: %i\n", i_85, cast_84 [i_85]);
    }
    mclUnmap(ctx_0, buffer_2, cast_84);
    mclReleaseDeviceData(&(buffer_2));
    mclReleaseKernel(kernel_7);
    mclReleaseProgram(program_1);
    mclReleaseContext(&(ctx_0));
}