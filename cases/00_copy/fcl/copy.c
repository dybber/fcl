#include <stdio.h>
#include <sys/time.h>
#include <mcl.h>
#include <fcl.h>
int main() {
    mclContext ctx_0 = mclInitialize(2);
    cl_program program_1 = mclBuildProgram(ctx_0, "copy.cl");
    mclDeviceData buffer_2 = mclAllocDevice(ctx_0, MCL_RW, sizeof(int), ((4194304 / 256) * 256));
    int ub_2_3 = (4194304 / 256);
    cl_kernel kernel_7 = mclCreateKernel(program_1, "kernel_4");
    mclSetKernelArg(kernel_7, 0, 2048, NULL);
    mclSetKernelArg(kernel_7, 1, sizeof(cl_mem), &(buffer_2.data));
    int arg_34 = ub_2_3;
    mclSetKernelArg(kernel_7, 2, sizeof(int), &(arg_34));
    mclInvokeKernel(ctx_0, kernel_7, (256 * 2048), 256);
    mclFinish(ctx_0);
    void* hostMMap_35 = mclMap(ctx_0, buffer_2, CL_MAP_READ, (10 * sizeof(int)));
    int* cast_36 = ((int*) hostMMap_35);
    int ub_38 = 10;
    for (int i_37 = 0; i_37 < ub_38; i_37++) {
        printf("%i: %i\n", i_37, cast_36 [i_37]);
    }
    mclUnmap(ctx_0, buffer_2, cast_36);
    mclReleaseDeviceData(&(buffer_2));
    mclReleaseKernel(kernel_7);
    mclReleaseProgram(program_1);
    mclReleaseContext(&(ctx_0));
}