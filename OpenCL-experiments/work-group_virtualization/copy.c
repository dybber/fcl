#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <sys/time.h>
#include <mcl.h>

#define BLOCK_DIM 16
#define BLOCK_SIZE (BLOCK_DIM*BLOCK_DIM)
#define NUM_ITERATIONS 1000

#define timediff(old, new) (((double)new.tv_sec + 1.0e-6 * (double)new.tv_usec) \
                            - ((double)old.tv_sec + 1.0e-6 * (double)old.tv_usec))

void copy1D(mclContext ctx,
            cl_kernel kernel,
            mclDeviceData output,
            mclDeviceData input,
            int num_elems,
            int blocks,
            int useSM) {
    cl_int sharedMemory = BLOCK_SIZE * sizeof(cl_int);
    mclSetKernelArg(kernel, 0, sizeof(cl_mem), &output.data);
    mclSetKernelArg(kernel, 1, sizeof(cl_mem), &input.data);
    mclSetKernelArg(kernel, 2, sizeof(cl_int), &num_elems);
    if(useSM) {
      mclSetKernelArg(kernel, 3, sharedMemory, NULL); // local/shared memory
    }
    mclInvokeKernel(ctx, kernel, blocks*BLOCK_SIZE, BLOCK_SIZE);
}

void test_copy_kernel(mclContext ctx, cl_program p, char* kernelName, int useSM, unsigned int num_elems, unsigned int blocks) {
    cl_kernel copyKernel = mclCreateKernel(p, kernelName);

    int* input = (int*)calloc(num_elems, sizeof(int));
    for (int i = 0; i < num_elems; i++) {
      input[i] = rand();
    }

    mclDeviceData buf = mclDataToDevice(ctx, MCL_R, num_elems, sizeof(int), input);
    mclDeviceData outbuf = mclAllocDevice(ctx, MCL_W, num_elems, sizeof(int));

    // Also serves as warm-up
    copy1D(ctx, copyKernel, outbuf, buf, num_elems, blocks, useSM);
    mclFinish(ctx);

    cl_int* out = (cl_int*)mclMap(ctx, outbuf, CL_MAP_READ, num_elems * sizeof(cl_int));

    cl_int num_errors = 0;
    for (int i = 0; i < num_elems; i++) {
      if (out[i] != input[i]) {
        num_errors++;
        if(num_errors > 10) {
          printf("More than 10 errors found. Stopping comparison.\n");
          break;
        } else {
          printf("Error: out[%d]!=input[%d] (%d, %d)\n", i, i, out[i], input[i]);
        }
      }
    }
    if (num_errors == 0) {
      printf("PASSED validation. No errors.\n");
    }
    mclUnmap(ctx, outbuf, out);

    if (num_errors == 0) {
      printf("Timing on %d executions\n", NUM_ITERATIONS);
      struct timeval begin, end;
      gettimeofday(&begin, NULL);
      for (int i = 0; i < NUM_ITERATIONS; ++i) {
        copy1D(ctx, copyKernel, outbuf, buf, num_elems, blocks, useSM);
      }
      mclFinish(ctx);
      gettimeofday(&end, NULL);

      double time = (timediff(begin, end))/(double)NUM_ITERATIONS;

      printf("%s: Throughput = %.4f GB/s, Time = %.5f s, Size = %u ints, Workgroup size = %u, numWgs = %u, blocksPerWg = %u\n",
             kernelName,
             (1.0e-9 * (double)(num_elems * sizeof(float))/time),
             time, num_elems, BLOCK_SIZE, blocks, num_elems/BLOCK_SIZE/blocks);
    }

    mclReleaseDeviceData(&buf);
    mclReleaseDeviceData(&outbuf);
    mclReleaseKernel(copyKernel);
}

int main () {
  srand(time(NULL));
  mclContext ctx = mclInitialize(0);
  cl_program p = mclBuildProgram(ctx, "copy.cl");

  unsigned int n = 2048 * 2048; /* Must be a multiple of BLOCK_SIZE */

  // Launch 'n' threads, each copying a single element
  test_copy_kernel(ctx, p, "simple_copy1D", 0, n, n / BLOCK_SIZE);
  test_copy_kernel(ctx, p, "shared_copy1D", 1, n, n / BLOCK_SIZE);

  // Launch 64*BLOCK_SIZE = 16384 threads, 
  // each copying n / 64 / BLOCK_SIZE = 256 elements
  test_copy_kernel(ctx, p, "simple_copy1D_virt", 0, n, 4096);
  test_copy_kernel(ctx, p, "simple_copy1D_virt_strength_reduced", 0, n, 4096);

  mclReleaseProgram(p);

  mclReleaseContext(&ctx);
}
