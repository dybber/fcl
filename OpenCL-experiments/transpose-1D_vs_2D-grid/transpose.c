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

void transpose1D(mclContext ctx,
                 cl_kernel kernel,
                 mclDeviceData output,
                 mclDeviceData input,
                 int offset,
                 int width,
                 int height) {
  cl_int sharedMemory = BLOCK_DIM * (BLOCK_DIM + 1) * sizeof(cl_int);
  mclSetKernelArg(kernel, 0, sizeof(cl_mem), &output.data);
  mclSetKernelArg(kernel, 1, sizeof(cl_mem), &input.data);
  mclSetKernelArg(kernel, 2, sizeof(cl_int), &offset);
  mclSetKernelArg(kernel, 3, sizeof(cl_int), &width);
  mclSetKernelArg(kernel, 4, sizeof(cl_int), &height);
  mclSetKernelArg(kernel, 5, sharedMemory, NULL); // local/shared memory

  mclInvokeKernel(ctx, kernel, width*height, BLOCK_SIZE);
}

void transpose2D(mclContext ctx,
                 cl_kernel kernel,
                 mclDeviceData output,
                 mclDeviceData input,
                 int offset,
                 int width,
                 int height) {
    cl_int sharedMemory = BLOCK_DIM * (BLOCK_DIM + 1) * sizeof(cl_int);
    mclSetKernelArg(kernel, 0, sizeof(cl_mem), &output.data);
    mclSetKernelArg(kernel, 1, sizeof(cl_mem), &input.data);
    mclSetKernelArg(kernel, 2, sizeof(cl_int), &offset);
    mclSetKernelArg(kernel, 3, sizeof(cl_int), &width);
    mclSetKernelArg(kernel, 4, sizeof(cl_int), &height);
    mclSetKernelArg(kernel, 5, sharedMemory, NULL); // local/shared memory
    mclInvokeKernel2D(ctx, kernel, width, height,
                      BLOCK_DIM, BLOCK_DIM);
}

void test_transpose1D_kernel(mclContext ctx, cl_program p, char* kernelName, unsigned int width, unsigned int height) {
    cl_kernel transposeKernel = mclCreateKernel(p, kernelName);

    int num_elems = width * height;

    int* input = (int*)calloc(num_elems, sizeof(int));
    for (int i = 0; i < num_elems; i++) {
      input[i] = i;
    }

    // Create transposed matrix as reference
    int* expected = (int*)calloc(num_elems, sizeof(int));
    for( unsigned int y = 0; y < height; ++y) {
        for( unsigned int x = 0; x < width; ++x) {
            expected[(x * height) + y] = input[(y * width) + x];
        }
    }

    mclDeviceData buf = mclDataToDevice(ctx, MCL_R, num_elems, sizeof(int), input);
    mclDeviceData outbuf = mclAllocDevice(ctx, MCL_W, num_elems, sizeof(int));

    // Also serves as warm-up
    transpose1D(ctx, transposeKernel, outbuf, buf, 0, width, height);
    mclFinish(ctx);

    cl_int* out = (cl_int*)mclMap(ctx, outbuf, CL_MAP_READ, num_elems * sizeof(cl_int));

    cl_int num_errors = 0;
    for (int i = 0; i < num_elems; i++) {
      if (out[i] != expected[i]) {
        num_errors++;
        if(num_errors > 10) {
          printf("More than 10 errors found. Stopping comparison.\n");
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

    if (num_errors == 0) {
      printf("Timing on %d executions\n", NUM_ITERATIONS);
      struct timeval begin, end;
      gettimeofday(&begin, NULL);
      for (int i = 0; i < NUM_ITERATIONS; ++i) {
        transpose1D(ctx, transposeKernel, outbuf, buf, 0, width, height);
      }
      mclFinish(ctx);
      gettimeofday(&end, NULL);

      double time = (timediff(begin, end))/(double)NUM_ITERATIONS;

      printf("Stats for %s, Throughput = %.4f GB/s, Time = %.5f s, Size = %u integers, Workgroup = %u\n", kernelName,
             (1.0e-9 * (double)(num_elems * sizeof(int))/time),
             time, num_elems, BLOCK_SIZE);

    }

    mclReleaseDeviceData(&buf);
    mclReleaseDeviceData(&outbuf);
    mclReleaseKernel(transposeKernel);
}

void test_transpose2D_kernel(mclContext ctx, cl_program p, char* kernelName, int useSM,
                          unsigned int width, unsigned int height) {
    cl_kernel transposeKernel = mclCreateKernel(p, kernelName);

    const size_t num_elems = width * height;

    int* input = (int*)calloc(num_elems, sizeof(int));
    for (int i = 0; i < num_elems; i++) {
      input[i] = i;
    }

    // Create transposed matrix as reference
    int* expected = (int*)calloc(num_elems, sizeof(int));
    for( unsigned int y = 0; y < height; ++y) {
        for( unsigned int x = 0; x < width; ++x) {
            expected[(x * height) + y] = input[(y * width) + x];
        }
    }

    mclDeviceData buf = mclDataToDevice(ctx, MCL_R, num_elems, sizeof(int), input);
    mclDeviceData outbuf = mclAllocDevice(ctx, MCL_W, num_elems, sizeof(int));

    // Also serves as warm-up
    transpose2D(ctx, transposeKernel, outbuf, buf, 0, width, height);
    mclFinish(ctx);

    cl_int* out = (cl_int*)mclMap(ctx, outbuf, CL_MAP_READ, num_elems * sizeof(cl_int));

    cl_int num_errors = 0;
    for (int i = 0; i < num_elems; i++) {
      if (out[i] != expected[i]) {
        num_errors++;
        if(num_errors > 10) {
          printf("More than 10 errors found. Stopping comparison.\n");
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

    if (num_errors == 0) {
      printf("Timing on %d executions\n", NUM_ITERATIONS);
      struct timeval begin, end;
      gettimeofday(&begin, NULL);
      for (int i = 0; i < NUM_ITERATIONS; ++i) {
        transpose2D(ctx, transposeKernel, outbuf, buf, 0, width, height);
      }
      mclFinish(ctx);
      gettimeofday(&end, NULL);

      double time = (timediff(begin, end))/(double)NUM_ITERATIONS;

      printf("Stats for %s, Throughput = %.4f GB/s, Time = %.5f s, Size = %u integers, Workgroup = %u\n", kernelName,
             (1.0e-9 * (double)(width * height * sizeof(int))/time),
             time, (width * height), BLOCK_DIM * BLOCK_DIM);
    }

    mclReleaseDeviceData(&buf);
    mclReleaseDeviceData(&outbuf);
    mclReleaseKernel(transposeKernel);
}


int main () {
  srand(time(NULL));
  mclContext ctx = mclInitialize(0);
  cl_program p = mclBuildProgram(ctx, "transpose.cl");

  // Launch a grid of 2048 by 2048 threads, each transposeing a single
  // element
  test_transpose1D_kernel(ctx, p, "transpose1D", 2048, 2048);
  //test_transpose2D_kernel(ctx, p, "transpose", 1, 2048, 2048);

  mclReleaseProgram(p);
  mclReleaseContext(&ctx);
}
