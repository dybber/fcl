// OpenCL-implementation of copy using shared-memory, described
// here in CUDA here:
// http://devblogs.nvidia.com/parallelforall/using-shared-memory-cuda-cc/
// Kernel code from NVIDIA SDK

#include <stdio.h>
#include <sys/time.h>
#include <mcl.h>

#define BLOCK_DIM 16
#define BLOCK_SIZE (BLOCK_DIM*BLOCK_DIM)
#define NUM_ITERATIONS 100

#define timediff(old, new) (((double)new.tv_sec + 1.0e-6 * (double)new.tv_usec) \
                            - ((double)old.tv_sec + 1.0e-6 * (double)old.tv_usec))

void transpose(mclContext ctx,
               cl_kernel kernel,
               int splitSize,
               int rows,
               int cols,
               mclDeviceData input,
               int size,
               mclDeviceData output,
               int blocks) {
    mclSetKernelArg(kernel, 0, sizeof(cl_int) * 1024, NULL);
    mclSetKernelArg(kernel, 1, sizeof(cl_int), &splitSize);
    mclSetKernelArg(kernel, 2, sizeof(cl_int), &rows);
    mclSetKernelArg(kernel, 3, sizeof(cl_int), &cols);
    mclSetKernelArg(kernel, 4, sizeof(cl_mem), &input.data);
    mclSetKernelArg(kernel, 5, sizeof(cl_int), &size);
    mclSetKernelArg(kernel, 6, sizeof(cl_mem), &output.data);
    mclInvokeKernel(ctx, kernel, blocks * BLOCK_SIZE, BLOCK_SIZE);
}

void test_transpose_kernel(mclContext ctx, cl_program p, char* kernelName, int splitSize, int rows, int cols, int blocks) {
    cl_kernel transposeKernel = mclCreateKernel(p, kernelName);

    const size_t num_elems = rows*cols;

    int* input = (int*)calloc(num_elems, sizeof(int));
    int* expected_out = (int*)calloc(num_elems, sizeof(int));
    int ix = 0;
    for (int i = 0; i < rows; i++) {
      for (int j = 0; j < cols; j++) {
        input[ix] = ix;
        expected_out[j*rows + i] = ix;
        ix++;
      }
    }

    mclDeviceData buf = mclDataToDevice(ctx, MCL_R, num_elems, sizeof(int), input);
    mclDeviceData outbuf = mclAllocDevice(ctx, MCL_W, num_elems, sizeof(int));

    // Also serves as warm-up
    transpose(ctx, transposeKernel, splitSize, rows, cols, buf, num_elems, outbuf, blocks);
    mclFinish(ctx);

    cl_int* out = (cl_int*)mclMap(ctx, outbuf, CL_MAP_READ, num_elems * sizeof(cl_int));

    cl_int num_errors = 0;
    for (int i = 0; i < rows; i++) {
      for (int j = 0; j < cols; j++) {
        int ix = i*cols + j;
        if (out[ix] != expected_out[ix]) {
          num_errors++;
          if(num_errors > 10) {
            printf("More than 10 errors found. Stopping comparison.\n");
            exit(-1);
          } else {
            printf("Error: out[%d,%d]!=expected_out[%d,%d] (%d, %d)\n", i, j, i, j, out[ix], expected_out[ix]);
          }
        }
      }
    }
    if (num_errors == 0) {
      printf("PASSED validation. No errors.\n");
    }
    mclUnmap(ctx, outbuf, out);
    mclFinish(ctx);

    if (num_errors == 0) {
      printf("Timing on %d executions\n", NUM_ITERATIONS);
      struct timeval begin, end;
      gettimeofday(&begin, NULL);
      for (int i = 0; i < NUM_ITERATIONS; ++i) {
          transpose(ctx, transposeKernel, splitSize, rows, cols, buf, num_elems, outbuf, blocks);
      }
      mclFinish(ctx);
      gettimeofday(&end, NULL);

      double time = (timediff(begin, end))/(double)NUM_ITERATIONS;

      printf("Stats for %s, Throughput = %.4f GB/s, Time = %.5f s, Size = %lu fp32 elements, Workgroup = %u\n", kernelName,
             (1.0e-9 * (double)(2 * num_elems * sizeof(float))/time),
             time, num_elems, BLOCK_SIZE);
    }

    mclReleaseDeviceData(&buf);
    mclReleaseDeviceData(&outbuf);
    mclReleaseKernel(transposeKernel);
}

int main () {
  mclContext ctx = mclInitialize(0);
  cl_program p = mclBuildProgram(ctx, "transpose.cl");

  test_transpose_kernel(ctx, p, "transposeChunked", BLOCK_DIM, 2048, 2048, 256);

  mclReleaseProgram(p);
  mclReleaseContext(&ctx);
}
