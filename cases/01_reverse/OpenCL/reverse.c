// OpenCL-reimplementation of reversal using shared-memory, described
// here:
// http://devblogs.nvidia.com/parallelforall/using-shared-memory-cuda-cc/

#include <stdio.h>
#include <getopt.h>
#include <sys/time.h>
#include <mcl.h>

#define NUM_ITERATIONS 100
#define timediff(old, new) (((double)new.tv_sec + 1.0e-6 * (double)new.tv_usec) \
                            - ((double)old.tv_sec + 1.0e-6 * (double)old.tv_usec))

void reverse(mclContext ctx,
             int kernelNo,
             cl_kernel kernel,
             mclDeviceData output,
             mclDeviceData input,
             int nThreads, int nBlocks) {

    mclSetKernelArg(kernel, 0, sizeof(cl_mem), &output.data);
    mclSetKernelArg(kernel, 1, sizeof(cl_int), &input.data);
    if (kernelNo != 0) {
      // dynamic
      mclSetKernelArg(kernel, 2, sizeof(cl_int)*nThreads, NULL); // local/shared memory
    }

    mclInvokeKernel(ctx, kernel, nThreads*nBlocks, nThreads);
}

int runReverse(int kernelNo) {
    const int num_elems = 2048*2048;

    char* kernelName;
    if(kernelNo == 0) {
      kernelName = "staticReverse";
    } else {
      kernelName = "dynamicReverse";
    }

    mclContext ctx = mclInitialize(0);
    cl_program p = mclBuildProgram(ctx, "reverse.cl");
    cl_kernel kernel = mclCreateKernel(p, kernelName);

    int* input = (int*)calloc(num_elems, sizeof(int));
    int* expected = (int*)calloc(num_elems, sizeof(int));
    for (int i = 0; i < num_elems; i++) {
      input[i] = i;
      expected[i] = num_elems-i-1;
    }

    /////// Test reverse ///////
    mclDeviceData buf = mclDataToDevice(ctx, MCL_RW, num_elems, sizeof(int), input);
    mclDeviceData outbuf = mclAllocDevice(ctx, MCL_W, num_elems, sizeof(int));

    int nThreads = 256;
    int nBlocks = (num_elems + nThreads - 1)/nThreads;

    printf("Debug: num groups: %d \n", nBlocks);

    // This also serves as warm-up
    reverse(ctx, kernelNo, kernel, outbuf, buf, nThreads, nBlocks);
    mclFinish(ctx);

    // Check results
    cl_int* out = (cl_int*)mclMap(ctx, outbuf, CL_MAP_READ, sizeof(cl_int));
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

    mclUnmap(ctx, buf, out);

    // Time 100 calls
    struct timeval begin, end;
    gettimeofday(&begin, NULL);
    for (int i = 0; i < NUM_ITERATIONS; ++i) {
      reverse(ctx, kernelNo, kernel, outbuf, buf, nThreads, nBlocks);
    }
    mclFinish(ctx);
    gettimeofday(&end, NULL);
    double time = (timediff(begin, end))/(double)NUM_ITERATIONS;

    if (num_errors == 0) {
      printf("Stats for %s, Throughput = %.4f GB/s, Time = %.5f s, Size = %u fp32 elements, Workgroup = %u\n", kernelName,
             (1.0e-9 * (double)(num_elems * sizeof(int))/time),
             time, num_elems, nThreads);

    }

    mclReleaseDeviceData(&buf);
    mclReleaseKernel(kernel);
    mclReleaseProgram(p);
    mclReleaseContext(&ctx);

    return 0;
}

int main(int argc, char* const * argv) {
  int c;
  switch(c = getopt(argc, argv, "k:")) {
  case 'k':
    runReverse(atoi(optarg));
    break;
  default:
    fprintf(stderr, "err %c\n", c);
    exit(-1);
  }

  return 0;
}
