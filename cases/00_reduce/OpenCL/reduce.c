#include <stdio.h>
#include <getopt.h>
#include <sys/time.h>
#include <mcl.h>

#define MAX_BLOCK_DIM_SIZE 65535
#define NUM_ITERATIONS 100
#define timediff(old, new) (((double)new.tv_sec + 1.0e-6 * (double)new.tv_usec) \
                            - ((double)old.tv_sec + 1.0e-6 * (double)old.tv_usec))

// Recursively invoke reduction kernel
void reduce(mclContext ctx, int kernelNo, cl_kernel kernel,
            mclDeviceData d_input, mclDeviceData d_output,
            int size, int nThreads, int nBlocks) {

    /* printf("  numBlocks: %d\n", nBlocks); */
    /* printf("  numThreads: %d\n", nThreads); */
    /* printf("  size: %d\n", size); */

    mclSetKernelArg(kernel, 0, sizeof(cl_mem), &d_input.data);
    mclSetKernelArg(kernel, 1, sizeof(cl_mem), &d_output.data);
    mclSetKernelArg(kernel, 2, sizeof(cl_int), &size);
    mclSetKernelArg(kernel, 3, sizeof(cl_int) * nThreads, NULL); /* Shared memory */
    mclInvokeKernel(ctx, kernel, nThreads*nBlocks, nThreads);

    if (nBlocks > 1) {
      int new_nBlocks;
      if (kernelNo < 3)
        new_nBlocks = (nBlocks + nThreads - 1) / nThreads;
      else
        new_nBlocks = (nBlocks + (nThreads*2-1)) / (nThreads*2);

      int kernelNo_ = (kernelNo == 6) ? 5 : kernelNo;
      /* int new_nBlocks = (size + nThreads - 1) / nThreads; */
      reduce(ctx, kernelNo_, kernel, d_output, d_output, nBlocks, nThreads, new_nBlocks);
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

void getNumBlocksAndThreads(int whichKernel, int n, int* blocks, int* threads) {
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
        

    /* if (whichKernel == 6) */
    /*     blocks = MIN(maxBlocks, blocks); */
}

void runReduce(int kernelNo) {
    int size = 1<<24;    // number of elements to reduce

    char kernelName[20];
    snprintf(kernelName, sizeof(kernelName), "reduce%d", kernelNo);

    mclContext ctx = mclInitialize(0);    
    cl_program p = mclBuildProgram(ctx, "./oclReduction_kernel.cl");
    cl_kernel kernel = mclCreateKernel(p, kernelName);

    printf("Creating input array \n");
    int* input = (int*)calloc(size, sizeof(int));
    int expected_out = 0;
    for (int i = 0; i < size; i++) {
      int v = (int)(rand() & 0xFF);
      input[i] = v;
      expected_out += v;
    }

    /* int nThreads = 128; // threads per block */
    /* int nBlocks = (size + nThreads - 1) / nThreads; */
    int nThreads, nBlocks;
    getNumBlocksAndThreads(kernelNo, size, &nBlocks, &nThreads);

    printf("Copy input data to device memory \n");
    mclDeviceData input_buf = mclDataToDevice(ctx, MCL_RW, MCL_INT, input, size);

    printf("Allocate output buffer \n");
    mclDeviceData out_buf = mclAllocDevice(ctx, MCL_RW, MCL_INT, nBlocks);

    printf("Debug: Calling kernel %s \n", kernelName);
    reduce(ctx, kernelNo, kernel, input_buf, out_buf, size, nThreads, nBlocks);
    mclFinish(ctx);

    // Check results
    cl_int* out = (cl_int*)mclMap(ctx, out_buf, CL_MAP_READ, sizeof(cl_int));
    printf("output:   %d\n", *out);
    printf("expected: %d\n", expected_out);
    mclUnmap(ctx, out_buf, out);

    // Time 100 calls
    struct timeval begin, end;
    gettimeofday(&begin, NULL);
    for (int i = 0; i < NUM_ITERATIONS; ++i) {
      reduce(ctx, kernelNo, kernel, input_buf, out_buf, size, nThreads, nBlocks);
    }
    mclFinish(ctx);
    gettimeofday(&end, NULL);
    double time = (timediff(begin, end))/(double)NUM_ITERATIONS;

    printf("Stats for %s, Throughput = %.4f GB/s, Time = %.5f s, Size = %u fp32 elements, Workgroup = %u\n", kernelName,
             (1.0e-9 * (double)(size * sizeof(int))/time),
             time, size, nThreads);

    mclReleaseKernel(kernel);
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
    fprintf(stderr, "err %c\n", c);
    exit(-1);
  }

  return 0;
}
