// OpenCL-implementation of copy using shared-memory, described
// here in CUDA here:
// http://devblogs.nvidia.com/parallelforall/using-shared-memory-cuda-cc/
// Kernel code from NVIDIA SDK

#include <stdio.h>
#include <sys/time.h>
#include <mcl.h>

#define BLOCK_SIZE 256
#define NUM_ITERATIONS 100

#define timediff(old, new) (((double)new.tv_sec + 1.0e-6 * (double)new.tv_usec) \
                            - ((double)old.tv_sec + 1.0e-6 * (double)old.tv_usec))

void reverse(mclContext ctx,
             cl_kernel kernel,
             int numWgs,
             mclDeviceData input,
             int size,
             mclDeviceData output) {
    mclSetKernelArg(kernel, 0, sizeof(cl_int) * BLOCK_SIZE, NULL);
    mclSetKernelArg(kernel, 1, sizeof(cl_mem), &input.data);
    mclSetKernelArg(kernel, 2, sizeof(cl_int), &size);
    mclSetKernelArg(kernel, 3, sizeof(cl_mem), &output.data);
    mclInvokeKernel(ctx, kernel, numWgs * BLOCK_SIZE, BLOCK_SIZE);
}

void test_reverse_kernel(mclContext ctx, cl_program p, char* kernelName, unsigned int num_elems, int blocks) {
    cl_kernel revKernel = mclCreateKernel(p, kernelName);

    /* int wgsize = 256; */
    /* int numWgs = (num_elems + wgsize - 1) / wgsize; */

    int* input = (int*)calloc(num_elems, sizeof(int));
    int* expected_out = (int*)calloc(num_elems, sizeof(int));
    for (int i = 0; i < num_elems; i++) {
      input[i] = i;
      expected_out[i] = num_elems-1-i;
    }

    mclDeviceData buf = mclDataToDevice(ctx, MCL_R, num_elems, sizeof(int), input);
    mclDeviceData outbuf = mclAllocDevice(ctx, MCL_W, num_elems, sizeof(int));

    // Also serves as warm-up
    reverse(ctx, revKernel, blocks, buf, num_elems, outbuf);
    mclFinish(ctx);

    cl_int* out = (cl_int*)mclMap(ctx, outbuf, CL_MAP_READ, num_elems * sizeof(cl_int));

    cl_int num_errors = 0;
    for (int i = 0; i < num_elems; i++) {
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
    mclUnmap(ctx, outbuf, out);

    if (num_errors == 0) {
      printf("Timing on %d executions\n", NUM_ITERATIONS);
      struct timeval begin, end;
      gettimeofday(&begin, NULL);
      for (int i = 0; i < NUM_ITERATIONS; ++i) {
        reverse(ctx, revKernel, blocks, buf, num_elems, outbuf);
      }
      mclFinish(ctx);
      gettimeofday(&end, NULL);
      
      

      double avgtime = (timediff(begin, end))/(double)NUM_ITERATIONS;

      printf("Stats for %s, Throughput = %.4f GB/s, Average time = %.5f s, Size = %u integers, Workgroup = %u\n", kernelName,
             (1.0e-9 * (double)(num_elems * sizeof(float))/avgtime),
             avgtime, num_elems, blocks * BLOCK_SIZE);

    }

    mclReleaseDeviceData(&buf);
    mclReleaseDeviceData(&outbuf);
    mclReleaseKernel(revKernel);
}

int main () {
  mclContext ctx = mclInitialize(0);
  cl_program p = mclBuildProgram(ctx, "reverse.cl");

  int n = 2048*2048 * 2;

  test_reverse_kernel(ctx, p, "reverseKernel", n, 4096);

  mclReleaseProgram(p);
  mclReleaseContext(&ctx);
}
