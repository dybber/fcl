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
    mclSetKernelArg(kernel, 0, sizeof(cl_int) * 2048, NULL); /* Shared memory */
    mclSetKernelArg(kernel, 1, sizeof(cl_mem), &d_input.data);
    mclSetKernelArg(kernel, 2, sizeof(cl_int), &size);
    mclSetKernelArg(kernel, 3, sizeof(cl_mem), &d_output.data);

    mclInvokeKernel(ctx, kernel, nThreads*nBlocks, nThreads);
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

void getNumBlocksAndThreads(int maxBlocks, int n, int* blocks, int* threads) {
    int maxThreads = 128;

    *threads = (n < maxThreads) ? nextPow2(n) : maxThreads;
    *blocks = (n + *threads - 1) / *threads;
}

void runReduce(char* kernelName) {
    int size = 1 << 24;    // number of elements to reduce

    mclContext ctx = mclInitialize(0);    
    cl_program p = mclBuildProgram(ctx, "reduce.cl");
    cl_kernel kernel1 = mclCreateKernel(p, kernelName);

    int maxBlocks;
    clGetKernelWorkGroupInfo(kernel1, ctx.device_id, CL_KERNEL_WORK_GROUP_SIZE, sizeof(size_t), &maxBlocks, NULL);


    int nThreads, nBlocks;
    getNumBlocksAndThreads(maxBlocks, size, &nBlocks, &nThreads);


    int* input = (int*)calloc(size, sizeof(int));
    int* expected_out = (int*)calloc(nBlocks, sizeof(int));
    for (int i = 0; i < nBlocks/2; i++) {
      for (int j = 0; j < nThreads*2; j++) {
        //int v = (int)(rand() & 0xFF);
        int v = i*nThreads*2 + j;
        input[i*nThreads*2 + j] = v;
        expected_out[i] += v;
      }
    }

    mclDeviceData input_buf = mclDataToDevice(ctx, MCL_RW, sizeof(int), size, input);
    mclDeviceData out_buf = mclAllocDevice(ctx, MCL_RW, sizeof(int), nBlocks);

    printf("  blocks: %d\n", nBlocks);
    printf("  workgroup size: %d\n", nThreads);
    printf("  elements: %d\n", size);

    reduceOnce(ctx, kernel1, input_buf, out_buf, size, nThreads, nBlocks);
    mclFinish(ctx);

    // Check results
    cl_int* out = (cl_int*)mclMap(ctx, out_buf, CL_MAP_READ, nBlocks * sizeof(cl_int));


    cl_int num_errors = 0;
    for (int i = 0; i < nBlocks/2; i++) {
      if (out[i] != expected_out[i]) {
        num_errors++;
        if(num_errors > 10) {
          printf("More than 10 errors found. Stopping comparison.\n");
          break;
        } else {
          printf("Error: out[%d]!=expected_out[%d] (%d, %d)\n", i, i, out[i], expected_out[i]);
        }
      }
    }
    if (num_errors == 0) {
      printf("PASSED validation. No errors.\n");
    }

    /* printf("Partial sums:\n"); */
    /* for (int i = 0; i < nBlocks; i++) { */
    /*   printf("  %i:   %d\n", i, out[i]); */
    /* } */

    /* printf("Expected:\n"); */
    /* for (int i = 0; i < nBlocks; i++) { */
    /*   printf("  %i:   %d\n", i, expected_out[i]); */
    /* } */
    mclUnmap(ctx, out_buf, out);
    mclFinish(ctx);

    // Time 100 calls
    struct timeval begin, end;
    gettimeofday(&begin, NULL);
    for (int i = 0; i < NUM_ITERATIONS; ++i) {
        reduceOnce(ctx, kernel1, input_buf, out_buf, size, nThreads, nBlocks);
    }
    mclFinish(ctx);
    gettimeofday(&end, NULL);
    double time = (timediff(begin, end))/(double)NUM_ITERATIONS;

    printf("Stats for %s, Throughput = %.4f GB/s, Time = %.5f s, Size = %u fp32 elements, Workgroup = %u\n", kernelName,
             (1.0e-9 * (double)(size * sizeof(int))/time),
             time, size, nThreads);

    printf("\n");
    mclReleaseKernel(kernel1);
    mclReleaseProgram(p);
    mclReleaseDeviceData(&input_buf);
    mclReleaseDeviceData(&out_buf);
    mclReleaseContext(&ctx);
}

int main(int argc, char* const * argv) {
  runReduce("reduceAdd");

  return 0;
}
