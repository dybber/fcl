// OpenCL-reimplementation of reversal using shared-memory, described
// here:
// http://devblogs.nvidia.com/parallelforall/using-shared-memory-cuda-cc/

#include <stdio.h>
#include <sys/time.h>
#include <mcl.h>

#define BLOCK_DIM 16
#define NUM_ITERATIONS 100

#define timediff(old, new) (((double)new.tv_sec + 1.0e-6 * (double)new.tv_usec) \
                            - ((double)old.tv_sec + 1.0e-6 * (double)old.tv_usec))

unsigned long transpose(mclContext ctx,
                        cl_kernel kernel,
                        mclDeviceData output,
                        mclDeviceData input,
                        int offset,
                        int size_x,
                        int size_y) {
    cl_int sharedMemory = BLOCK_DIM * (BLOCK_DIM + 1) * sizeof(cl_int);
    mclSetKernelArg(kernel, 0, sizeof(cl_mem), &output.data);
    mclSetKernelArg(kernel, 1, sizeof(cl_mem), &input.data);
    mclSetKernelArg(kernel, 2, sizeof(cl_int), &offset);
    mclSetKernelArg(kernel, 3, sizeof(cl_int), &size_x);
    mclSetKernelArg(kernel, 4, sizeof(cl_int), &size_y);
    mclSetKernelArg(kernel, 5, sharedMemory, NULL); // local/shared memory
    unsigned long t = mclProfileKernel2D(ctx, kernel, size_x, size_y, 
                                         BLOCK_DIM, BLOCK_DIM);
    return t;
}

int main() {
    mclContext ctx = mclInitialize(0);
    cl_program p = mclBuildProgram(ctx, "transpose.cl");
    cl_kernel transposeKernel = mclCreateKernel(p, "transpose");

    const unsigned int size_x = 2048;
    const unsigned int size_y = 2048;

    const size_t num_elems = size_x * size_y;

    int* input = (int*)calloc(num_elems, sizeof(int));
    for (int i = 0; i < num_elems; i++) {
      input[i] = i;
    }

    // Create transposed matrix as reference
    int* expected = (int*)calloc(num_elems, sizeof(int));
    for( unsigned int y = 0; y < size_y; ++y) {
        for( unsigned int x = 0; x < size_x; ++x) {
            expected[(x * size_y) + y] = input[(y * size_x) + x];
        }
    }

    mclDeviceData buf = mclDataToDevice(ctx, MCL_R, sizeof(int), num_elems, input);
    mclDeviceData outbuf = mclAllocDevice(ctx, MCL_W, sizeof(int), num_elems);

    // Also serves as warm-up
    transpose(ctx, transposeKernel, outbuf, buf, 0, size_x, size_y);
    mclFinish(ctx);

    cl_int* out = (cl_int*)mclMap(ctx, outbuf, CL_MAP_READ, num_elems * sizeof(cl_int));

    cl_int num_errors = 0;
    for (int i = 0; i < num_elems; i++) {
      if (out[i] != expected[i]) {
        num_errors++;
        if(num_errors > 10) {
          printf("Debug: more than 10 errors found. Stopping comparison.\n");
          break;
        } else {
          printf("Error: out[%d]!=expected[%d] (%d, %d)\n", i, i, out[i], expected[i]);
        }
      }
    }
    if (num_errors == 0) {
      printf("PASSED validation. No errors.\n");
    }
    mclUnmap(ctx, outbuf, out);

    double sum_ms = 0.0;
    if (num_errors == 0) {
      printf("Timing on %d executions\n", NUM_ITERATIONS);
      /* struct timeval begin, end; */
      /* gettimeofday(&begin, NULL); */
      for (int i = 0; i < NUM_ITERATIONS; ++i) {
          unsigned long tdiff = transpose(ctx, transposeKernel, outbuf, buf, 0, size_x, size_y);
          sum_ms = sum_ms + (((double) tdiff) / 1000000.0);

      }
      mclFinish(ctx);
      /* gettimeofday(&end, NULL); */

      /* double time = (timediff(begin, end))/(double)NUM_ITERATIONS; */
      double time = sum_ms/(double)NUM_ITERATIONS;

      printf("Stats for %s, Throughput = %.4f GB/s, Time = %.5f ms, Size = %u fp32 elements, Workgroup = %u\n", "transpose",
             (1.0e-9 * (double)(size_x * size_y * sizeof(float))/(time/1000.0)),
             time, (size_x * size_y), BLOCK_DIM * BLOCK_DIM);

    }

    mclReleaseDeviceData(&buf);
    mclReleaseDeviceData(&outbuf);
    mclReleaseKernel(transposeKernel);
    mclReleaseProgram(p);
    mclReleaseContext(&ctx);

    return 0;
}
