#include <stdio.h>
#include <getopt.h>
#include <sys/time.h>
#include <mcl.h>

#define MAX_BLOCK_DIM_SIZE 65535
#define NUM_ITERATIONS 100
#define timediff(old, new) (((double)new.tv_sec + 1.0e-6 * (double)new.tv_usec) \
                            - ((double)old.tv_sec + 1.0e-6 * (double)old.tv_usec))

inline void reduceOnce(mclContext ctx, cl_kernel kernel,
                       mclDeviceData d_input, mclDeviceData d_output,
                       int size, int nThreads, int nBlocks) {
    mclSetKernelArg(kernel, 0, sizeof(cl_mem), &d_input.data);
    mclSetKernelArg(kernel, 1, sizeof(cl_mem), &d_output.data);
    mclSetKernelArg(kernel, 2, sizeof(cl_int), &size);
    mclSetKernelArg(kernel, 3, sizeof(cl_int) * nThreads, NULL); /* Shared memory */
    mclInvokeKernel(ctx, kernel, nThreads*nBlocks, nThreads);
}

inline void reduceIter(mclContext ctx, int kernelNo, cl_kernel k1, cl_kernel k2,
            mclDeviceData d_input, mclDeviceData d_output,
            int size, int nThreads, int nBlocks) {

    reduceOnce(ctx, k1, d_input, d_output, size, nThreads, nBlocks);

    while (nBlocks > 1) {
        size = nBlocks;

        if (kernelNo < 3)
            nBlocks = (nBlocks + nThreads - 1) / nThreads;
        else
            nBlocks = (nBlocks + (nThreads*2-1)) / (nThreads*2);

        reduceOnce(ctx, k2, d_output, d_output, size, nThreads, nBlocks);
    }
}

unsigned int nextPow2( unsigned int x ) {
    --x;
    x |= x >> 1;
    x |= x >> 2;
    x |= x >> 4;
    x |= x >> 8;
    x |= x >> 16;
    return ++x;
}

void getNumBlocksAndThreads(int whichKernel, int maxBlocks, int n, int* blocks, int* threads) {
    int maxThreads = 128;
    if (whichKernel < 3)
    {
        *threads = (n < maxThreads) ? nextPow2(n) : maxThreads;
        *blocks = (n + *threads - 1) / *threads;
    }
    else
    {
        *threads = (n < maxThreads*2) ? nextPow2((n + 1)/ 2) : maxThreads;
        *blocks = (n + (*threads * 2 - 1)) / (*threads * 2);
    }

    if (whichKernel == 6) {
      *blocks = (maxBlocks < *blocks) ? maxBlocks : *blocks;
    }
}

void runReduce(int kernelNo) {
    int size = 1<<24;    // number of elements to reduce

    char kernelName[20];
    snprintf(kernelName, sizeof(kernelName), "reduce%d", kernelNo);

    mclContext ctx = mclInitialize(0);    
    cl_program p = mclBuildProgram(ctx, "./oclReduction_kernel.cl");
    cl_kernel kernel1 = mclCreateKernel(p, kernelName);
    cl_kernel kernel2;
    if(kernelNo == 6) {
      kernel2 = mclCreateKernel(p, "reduce5");
    }
    else {
      kernel2 = mclCreateKernel(p, kernelName);
    }

    int maxBlocks;
    clGetKernelWorkGroupInfo(kernel1, ctx.device_id, CL_KERNEL_WORK_GROUP_SIZE, sizeof(size_t), &maxBlocks, NULL);

    int* input = (int*)calloc(size, sizeof(int));
    int expected_out = 0;
    for (int i = 0; i < size; i++) {
      int v = (int)(rand() & 0xFF);
      input[i] = v;
      expected_out += v;
    }

    int nThreads, nBlocks;
    getNumBlocksAndThreads(kernelNo, maxBlocks, size, &nBlocks, &nThreads);

    mclDeviceData input_buf = mclDataToDevice(ctx, MCL_RW, sizeof(int), size, input);
    mclDeviceData out_buf = mclAllocDevice(ctx, MCL_RW, sizeof(int), nBlocks);

    printf("  blocks: %d\n", nBlocks);
    printf("  workgroup size: %d\n", nThreads);
    printf("  elements: %d\n", size);

    reduceIter(ctx, kernelNo, kernel1, kernel2, input_buf, out_buf, size, nThreads, nBlocks);
    mclFinish(ctx);

    // Check results
    cl_int* out = (cl_int*)mclMap(ctx, out_buf, CL_MAP_READ, sizeof(cl_int));
    printf("  output:   %d\n", *out);
    printf("  expected: %d\n", expected_out);
    mclUnmap(ctx, out_buf, out);
    mclFinish(ctx);

    // Time 100 calls
    struct timeval begin, end;
    gettimeofday(&begin, NULL);
    for (int i = 0; i < NUM_ITERATIONS; ++i) {
        reduceIter(ctx, kernelNo, kernel1, kernel2, input_buf, out_buf, size, nThreads, nBlocks);
    }
    mclFinish(ctx);
    gettimeofday(&end, NULL);
    double time = (timediff(begin, end))/(double)NUM_ITERATIONS;

    printf("Stats for %s, Throughput = %.4f GB/s, Time = %.5f s, Size = %u fp32 elements, Workgroup = %u\n", kernelName,
             (1.0e-9 * (double)(size * sizeof(int))/time),
             time, size, nThreads);

    printf("\n");
    mclReleaseKernel(kernel1);
    mclReleaseKernel(kernel2);
    mclReleaseProgram(p);
    mclReleaseDeviceData(&input_buf);
    mclReleaseDeviceData(&out_buf);
    mclReleaseContext(&ctx);

}

int main(int argc, char* const * argv) {
  int c;
  switch(c = getopt(argc, argv, "k:")) {
  case 'k':
    runReduce(atoi(optarg));
    break;
  default:
    fprintf(stderr, "Remember -k0-6 argument to select kernel.\n");
    exit(-1);
  }

  return 0;
}
