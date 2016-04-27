// OpenCL-reimplementation of reversal using shared-memory, described
// here:
// http://devblogs.nvidia.com/parallelforall/using-shared-memory-cuda-cc/

#include <stdio.h>
#include <getopt.h>
#include <sys/time.h>
#include <mcl.h>

#define BLOCK_SIZE 256
#define NUM_ITERATIONS 1000
#define timediff(old, new) (((double)new.tv_sec + 1.0e-6 * (double)new.tv_usec) \
                            - ((double)old.tv_sec + 1.0e-6 * (double)old.tv_usec))

void reverse(mclContext ctx,
             cl_kernel kernel,
             mclDeviceData output,
             mclDeviceData input,
             int num_elems,
             int useSM) {

    mclSetKernelArg(kernel, 0, sizeof(cl_mem), &output.data);
    mclSetKernelArg(kernel, 1, sizeof(cl_mem), &input.data);
    if (useSM) {
      // dynamic
      mclSetKernelArg(kernel, 2, sizeof(cl_int)*BLOCK_SIZE, NULL); // local/shared memory
    }

    mclInvokeKernel(ctx, kernel, num_elems, BLOCK_SIZE);
}

int runReverse(mclContext ctx, cl_program p, char* kernelName, unsigned int num_elems, int useSM) {
    cl_kernel kernel = mclCreateKernel(p, kernelName);

    int* input = (int*)calloc(num_elems, sizeof(int));
    int* expected = (int*)calloc(num_elems, sizeof(int));
    for (int i = 0; i < num_elems; i++) {
      input[i] = i;
      expected[i] = num_elems-i-1;
    }

    /////// Test reverse ///////
    mclDeviceData buf = mclDataToDevice(ctx, MCL_R, num_elems, sizeof(int), input);
    mclDeviceData outbuf = mclAllocDevice(ctx, MCL_W, num_elems, sizeof(int));

    printf("Debug: num groups: %d \n", num_elems/BLOCK_SIZE);

    // This also serves as warm-up
    reverse(ctx, kernel, outbuf, buf, num_elems, useSM);
    mclFinish(ctx);

    // Check results
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

    // Time 100 calls
    struct timeval begin, end;
    gettimeofday(&begin, NULL);
    for (int i = 0; i < NUM_ITERATIONS; ++i) {
      reverse(ctx, kernel, outbuf, buf, num_elems, useSM);
    }
    mclFinish(ctx);
    gettimeofday(&end, NULL);
    double time = (timediff(begin, end))/(double)NUM_ITERATIONS;

    if (num_errors == 0) {
      printf("Stats for %s, Throughput = %.4f GB/s, Time = %.5f s, Size = %u fp32 elements, Workgroup = %u\n", kernelName,
             (1.0e-9 * (double)(2 * num_elems * sizeof(int))/time),
             time, num_elems, BLOCK_SIZE);

    }

    mclReleaseDeviceData(&buf);
    mclReleaseKernel(kernel);

    return 0;
}

int main() {
  int num_elems = 4096*4096;

  mclContext ctx = mclInitialize(0);
  cl_program p = mclBuildProgram(ctx, "reverse.cl");

  runReverse(ctx, p, "staticReverse", num_elems, 0);
  runReverse(ctx, p, "dynamicReverse", num_elems, 1);

  mclReleaseProgram(p);
  mclReleaseContext(&ctx);
}
