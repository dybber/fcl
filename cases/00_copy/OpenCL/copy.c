// OpenCL-implementation of copy using shared-memory, described
// here in CUDA here:
// http://devblogs.nvidia.com/parallelforall/using-shared-memory-cuda-cc/
// Kernel code from NVIDIA SDK

#include <stdio.h>
#include <sys/time.h>
#include <mcl.h>

#define BLOCK_DIM 16
#define NUM_ITERATIONS 1000

#define timediff(old, new) (((double)new.tv_sec + 1.0e-6 * (double)new.tv_usec) \
                            - ((double)old.tv_sec + 1.0e-6 * (double)old.tv_usec))

void copy(mclContext ctx,
          cl_kernel kernel,
          mclDeviceData output,
          mclDeviceData input,
          int offset,
          int size_x,
          int size_y,
          int useSM) {
    cl_int sharedMemory = BLOCK_DIM * (BLOCK_DIM + 1) * sizeof(cl_int);
    mclSetKernelArg(kernel, 0, sizeof(cl_mem), &output.data);
    mclSetKernelArg(kernel, 1, sizeof(cl_mem), &input.data);
    mclSetKernelArg(kernel, 2, sizeof(cl_int), &offset);
    mclSetKernelArg(kernel, 3, sizeof(cl_int), &size_x);
    mclSetKernelArg(kernel, 4, sizeof(cl_int), &size_y);
    if(useSM) {
      mclSetKernelArg(kernel, 5, sharedMemory, NULL); // local/shared memory
    }
    mclInvokeKernel2D(ctx, kernel, size_x, size_y, 
                                   BLOCK_DIM, BLOCK_DIM);
}

void test_copy_kernel(mclContext ctx, cl_program p, char* kernelName, int useSM) {
    cl_kernel copyKernel = mclCreateKernel(p, kernelName);

    const unsigned int size_x = 4096;
    const unsigned int size_y = 4096;

    const size_t num_elems = size_x * size_y;

    int* input = (int*)calloc(num_elems, sizeof(int));
    for (int i = 0; i < num_elems; i++) {
      input[i] = i;
    }

    mclDeviceData buf = mclDataToDevice(ctx, MCL_R, num_elems, sizeof(int), input);
    mclDeviceData outbuf = mclAllocDevice(ctx, MCL_W, num_elems, sizeof(int));

    // Also serves as warm-up
    copy(ctx, copyKernel, outbuf, buf, 0, size_x, size_y, useSM);
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
        copy(ctx, copyKernel, outbuf, buf, 0, size_x, size_y, useSM);
      }
      mclFinish(ctx);
      gettimeofday(&end, NULL);

      double time = (timediff(begin, end))/(double)NUM_ITERATIONS;

      printf("Stats for %s, Throughput = %.4f GB/s, Time = %.5f s, Size = %u integers, Workgroup = %u\n", kernelName,
             (1.0e-9 * (double)(2 * size_x * size_y * sizeof(float))/time),
             time, (size_x * size_y), BLOCK_DIM * BLOCK_DIM);

    }

    mclReleaseDeviceData(&buf);
    mclReleaseDeviceData(&outbuf);
    mclReleaseKernel(copyKernel);
}

int main () {
  mclContext ctx = mclInitialize(0);
  cl_program p = mclBuildProgram(ctx, "copy.cl");

  test_copy_kernel(ctx, p, "simple_copy", 0);
  test_copy_kernel(ctx, p, "shared_copy", 1);
  test_copy_kernel(ctx, p, "uncoalesced_copy", 0);

  mclReleaseProgram(p);
  mclReleaseContext(&ctx);
}
