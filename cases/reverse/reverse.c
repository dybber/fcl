// OpenCL-reimplementation of reversal using shared-memory, described
// here:
// http://devblogs.nvidia.com/parallelforall/using-shared-memory-cuda-cc/

#include <stdio.h>
#include <clu.h>

inline void check(cl_int errcode) {
  if (errcode != CL_SUCCESS) {
    printf("Error: %s\n", cluPrintError(errcode));
    exit(errcode);
  }
}

int main() {
    const int n = 64;
    cl_int status = cluInitialize(0);

    char src[8192];
    FILE *fd = fopen("kernels.cl","r");
    size_t srcsize = fread(src, sizeof src, 1, fd);
    fclose(fd);

    cl_program p = cluBuildSource(src, 0, 0, &status);
    if (0 != status) {
      const char* err = cluGetBuildErrors(p);
      printf("%s\n", err);
    }
    cl_kernel staticRev = clCreateKernel(p, "staticReverse", &status);
    cl_kernel dynamicRev = clCreateKernel(p, "dynamicReverse", &status);

    int input[n], expected_out[n];
    for (int i = 0; i < n; i++) {
      input[i] = i;
      expected_out[i] = n-i-1;
    }

    cl_mem buf = clCreateBuffer(CLU_CONTEXT, CL_MEM_READ_WRITE, n * sizeof(int), NULL, &status);
    check(status);

    // copy data to input buffer (device)
    status = clEnqueueWriteBuffer(CLU_DEFAULT_Q, buf, CL_TRUE, 0, n * sizeof(int), input, 0, NULL, NULL);
    check(status);

    // Static
    printf("Running staticReverse\n");
    status = clSetKernelArg(staticRev, 0, sizeof(cl_mem), &buf);
    check(status);
    status = clSetKernelArg(staticRev, 1, sizeof(n), &n);
    check(status);
    clu_enqueue_params params = CLU_DEFAULT_PARAMS;
    params.nd_range = cluNDRange1(n,n,0);
    status = cluEnqueue(staticRev, &params);
    check(status);

    cl_int* out = (cl_int*)clEnqueueMapBuffer(CLU_DEFAULT_Q, buf, CL_TRUE, CL_MAP_READ, 0, n*sizeof(cl_int), 0, 0, 0, &status);
//int out[n];
//status = clEnqueueReadBuffer(CLU_DEFAULT_Q, buf, CL_TRUE, 0, n * sizeof(int), out, 0, NULL, NULL);
    check(status);
    for (int i = 0; i < n; i++)
      if (out[i] != expected_out[i])
        printf("Error: out[%d]!=expected_out[%d] (%d, %d)\n", i, i, out[i], expected_out[i]);

    status = clEnqueueUnmapMemObject(CLU_DEFAULT_Q, buf, out, 0, 0, 0);


    status = clEnqueueWriteBuffer(CLU_DEFAULT_Q, buf, CL_TRUE, 0, n * sizeof(int), input, 0, NULL, NULL);
    check(status);
    // Dynamic
    printf("Running dynamicReverse\n");
    status = clSetKernelArg(dynamicRev, 0, sizeof(cl_mem), &buf);
    check(status);
    status = clSetKernelArg(dynamicRev, 1, n*sizeof(int), NULL); // local/shared memory
    check(status);
    status = clSetKernelArg(dynamicRev, 2, sizeof(int), &n);
    check(status);

    status = cluEnqueue(dynamicRev, &params);
    out = (cl_int*)clEnqueueMapBuffer(CLU_DEFAULT_Q, buf, CL_TRUE, CL_MAP_READ, 0, n*sizeof(cl_int), 0, 0, 0, &status);
    /* status = clEnqueueReadBuffer(CLU_DEFAULT_Q, buf, CL_TRUE, 0, n * sizeof(int), out, 0, NULL, NULL); */
    for (int i = 0; i < n; i++)
      if (out[i] != expected_out[i]) printf("Error: d[%d]!=r[%d] (%d, %d)\n", i, i, out[i], expected_out[i]);
    status = clEnqueueUnmapMemObject(CLU_DEFAULT_Q, buf, out, 0, 0, 0);

    cluRelease();

    return 0;
}
