// OpenCL-implementation of copy using shared-memory, described
// here in CUDA here:
// http://devblogs.nvidia.com/parallelforall/using-shared-memory-cuda-cc/
// Kernel code from NVIDIA SDK

#include <stdio.h>
#include <sys/time.h>
#include <mcl.h>

#define NUM_ITERATIONS 100

#define timediff(old, new) (((double)new.tv_sec + 1.0e-6 * (double)new.tv_usec) \
                            - ((double)old.tv_sec + 1.0e-6 * (double)old.tv_usec))

void reverse(mclContext ctx,
             cl_kernel kernel,
             int numWgs,
             int wgsize,
             mclDeviceData input,
             int size,
             mclDeviceData output,
             mclDeviceData sizeOut) {
    mclSetKernelArg(kernel, 0, sizeof(cl_int) * wgsize, NULL);
    mclSetKernelArg(kernel, 1, sizeof(cl_mem), &input.data);
    mclSetKernelArg(kernel, 2, sizeof(cl_int), &size);
    mclSetKernelArg(kernel, 3, sizeof(cl_mem), &output.data);
    mclSetKernelArg(kernel, 4, sizeof(cl_mem), &sizeOut.data);
    mclInvokeKernel(ctx, kernel, numWgs * wgsize, wgsize);
}

void test_copy_kernel(mclContext ctx, cl_program p, char* kernelName) {
    cl_kernel revKernel = mclCreateKernel(p, kernelName);

    const size_t num_elems = 512;

    int wgsize = 512;
    int numWgs = (num_elems + wgsize - 1) / wgsize;

    int* input = (int*)calloc(num_elems, sizeof(int));
    int* expected_out = (int*)calloc(num_elems, sizeof(int));
    for (int i = 0; i < num_elems; i++) {
      input[i] = i;
      expected_out[i] = num_elems-1-i;
    }

    mclDeviceData buf = mclDataToDevice(ctx, MCL_R, num_elems, sizeof(int), input);
    mclDeviceData outbuf = mclAllocDevice(ctx, MCL_W, num_elems, sizeof(int));
    mclDeviceData outlen = mclAllocDevice(ctx, MCL_W, 1, sizeof(int));

    // Also serves as warm-up
    reverse(ctx, revKernel, numWgs, wgsize, buf, num_elems, outbuf, outlen);
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
    mclUnmap(ctx, buf, out);

    if (num_errors == 0) {
      printf("Timing on %d executions\n", NUM_ITERATIONS);
      struct timeval begin, end;
      gettimeofday(&begin, NULL);
      for (int i = 0; i < NUM_ITERATIONS; ++i) {
        reverse(ctx, revKernel, numWgs, wgsize, buf, num_elems, outbuf, outlen);
      }
      mclFinish(ctx);
      gettimeofday(&end, NULL);

      double time = (timediff(begin, end))/(double)NUM_ITERATIONS;

      printf("Stats for %s, Throughput = %.4f GB/s, Time = %.5f s, Size = %lu fp32 elements, Workgroup = %u\n", kernelName,
             (1.0e-9 * (double)(num_elems * sizeof(float))/time),
             time, num_elems, numWgs * wgsize);

    }

    mclReleaseDeviceData(&buf);
    mclReleaseDeviceData(&outbuf);
    mclReleaseKernel(revKernel);
}

int main () {
  mclContext ctx = mclInitialize(0);
  cl_program p = mclBuildProgram(ctx, "reverse.cl");

  test_copy_kernel(ctx, p, "reverseGrid512");

  mclReleaseProgram(p);
  mclReleaseContext(&ctx);
}
