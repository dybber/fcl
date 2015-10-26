#include <stdio.h>
#include <clu.h>

#ifdef _DEBUG

#include <stdio.h>
void OCL_VALIDATE(cl_int in_status)
{
    if (in_status != CL_SUCCESS)
    {
      printf("%s(%d) OpenCL returned: %d (%s)\n", 
             __FILE__, __LINE__, in_status, cluPrintError(in_status));
    }
}
#else
#define OCL_VALIDATE(in_status) ((void) in_status)
#endif

void reduce(cl_kernel kernel, cl_mem d_input, cl_mem d_output,
            int size, int nThreads, int nBlocks) {
    #ifdef _DEBUG
    printf("  numBlocks: %d\n", nBlocks);
    printf("  numThreads: %d\n", nThreads);
    #endif

    cl_int status;
    // Static
    status = clSetKernelArg(kernel, 0, sizeof(cl_mem), &d_input);
    OCL_VALIDATE(status);
    status = clSetKernelArg(kernel, 1, sizeof(cl_mem), &d_output);
    OCL_VALIDATE(status);
    status = clSetKernelArg(kernel, 2, sizeof(cl_int), &size);
    OCL_VALIDATE(status);

    // allocates shared memory
    status = clSetKernelArg(kernel, 3, sizeof(cl_int) * nThreads, NULL);
    OCL_VALIDATE(status);

    clu_enqueue_params params = CLU_DEFAULT_PARAMS;
    params.nd_range = cluNDRange1(nBlocks*nThreads,nThreads,0);
    status = cluEnqueue(kernel, &params);
    OCL_VALIDATE(status);
    
    if (nBlocks > 1) {
      int new_nBlocks = (size + nThreads - 1) / nThreads;
      reduce(kernel, d_output, d_output, nBlocks, nThreads, new_nBlocks);
    }
}

int main() {
    int size = 1<<21;    // number of elements to reduce
    cl_int status = cluInitialize(0);

    char src[8192];
    FILE *fd = fopen("reduce0.cl","r");
    size_t srcsize = fread(src, sizeof(src), 1, fd);
    fclose(fd);

    cl_program p = cluBuildSource(src, 0, 0, &status);
    if (status != CL_SUCCESS) {
      const char* err = cluGetBuildErrors(p);
      printf("%s\n", err);
    }
    cl_kernel kernel = clCreateKernel(p, "reduce0", &status);
    OCL_VALIDATE(status);

    printf("Creating input array \n");
    int* input = (int*)malloc(size*sizeof(int));
    int expected_out = 0;
    for (int i = 0; i < size; i++) {
      int v = (int)(rand() & 0xFF);
      input[i] = v;
      expected_out += v;
    }
    cl_mem input_buf = clCreateBuffer(CLU_CONTEXT, CL_MEM_READ_WRITE, 
                                      size * sizeof(int), NULL, &status);
    OCL_VALIDATE(status);

    printf("Copy input data to device memory \n");
    // copy data to input buffer (device)
    status = clEnqueueWriteBuffer(CLU_DEFAULT_Q, input_buf, CL_TRUE, 0, 
                                  size * sizeof(int), input, 0, NULL, NULL);
    OCL_VALIDATE(status);

    printf("Allocate shared memory \n");
    // For the first iteration only
    int nThreads = 1024; // threads per block
    int nBlocks = (size + nThreads - 1) / nThreads;

    cl_mem out_buf = clCreateBuffer(CLU_CONTEXT, CL_MEM_READ_WRITE, 
                                    nBlocks * sizeof(int), NULL, &status);
    OCL_VALIDATE(status);

    printf("Reduce: \n");
    reduce(kernel, input_buf, out_buf, size, nThreads, nBlocks);

    cl_int* out = (cl_int*)clEnqueueMapBuffer(CLU_DEFAULT_Q, out_buf, CL_TRUE, CL_MAP_READ, 
                                              0, sizeof(cl_int), 0, 0, 0, &status);
    OCL_VALIDATE(status);
    
    printf("output:   %d\n", expected_out);
    printf("expected: %d\n", expected_out);

    status = clEnqueueUnmapMemObject(CLU_DEFAULT_Q, out_buf, out, 0, 0, 0);
    OCL_VALIDATE(status);

    cluRelease();

    return 0;
}
