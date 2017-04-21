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

cl_ulong matmul(mclContext ctx,
               cl_kernel kernel,
               mclDeviceData outputC,
               mclDeviceData inputA,
               mclDeviceData inputB,                   
               int widthA,
               int heightA,
               int widthB) {
    cl_int sharedMemory = BLOCK_DIM * (BLOCK_DIM + 1) * sizeof(double);
    cl_int localSize = BLOCK_DIM;
    mclSetKernelArg(kernel, 0, sizeof(cl_mem), &outputC.data);
    mclSetKernelArg(kernel, 1, sizeof(cl_mem), &inputA.data);
    mclSetKernelArg(kernel, 2, sizeof(cl_mem), &inputB.data);
    mclSetKernelArg(kernel, 3, sharedMemory, NULL); // local/shared memory
    mclSetKernelArg(kernel, 4, sharedMemory, NULL); // local/shared memory
    mclSetKernelArg(kernel, 5, sizeof(cl_int), &widthA);
    mclSetKernelArg(kernel, 6, sizeof(cl_int), &widthB);
    mclSetKernelArg(kernel, 7, sizeof(cl_int), &heightA);

    return mclProfileKernel2D(ctx, kernel, widthB, heightA, 
                              BLOCK_DIM, BLOCK_DIM);
}

/* from NVIDIA */
void matrixMul_reference(double* C, const double* A, const double* B, unsigned int hA, unsigned int wA, unsigned int wB)
{
    for (unsigned int i = 0; i < hA; ++i)
        for (unsigned int j = 0; j < wB; ++j) {
            double sum = 0;
            for (unsigned int k = 0; k < wA; ++k) {
              double a = A[i * wA + k];
              double b = B[k * wB + j];
              sum += a * b;
            }
            C[i * wB + j] = (double)sum;
        }
}

void print_matrix(double* M, unsigned int width, unsigned int height) {
    for (int i = 0; i < height; i++) {
      for (int j = 0; j < width; j++) {
       printf("%.7f,", M[i*width+j]);
      }
       printf("\n");
    }
}

int main() {
    mclContext ctx = mclInitialize(0);
    cl_program p = mclBuildProgram(ctx, "matrixMul.cl");
    cl_kernel matmulKernel = mclCreateKernel(p, "matrixMul");

    int sizeFactor = 10;
    
    const unsigned int wA = 80*sizeFactor;
    const unsigned int hA = 160*sizeFactor;
    const unsigned int wB = 80*sizeFactor;
    const unsigned int hB = wA;
    const unsigned int wC = wB;
    const unsigned int hC = hA;

    const size_t size_A = wA * hA;
    const size_t size_B = wB * hB;
    const size_t size_C = wC * hC;

    double* inputA = (double*)calloc(size_A, sizeof(double));
    double* inputB = (double*)calloc(size_B, sizeof(double));
    double* expected = (double*)calloc(size_C, sizeof(double));
    
    for (int i = 0; i < size_A; i++) {
      /* inputA[i] = (double)i; */
      inputA[i] = ((double)i) / ((double)size_A);
    }
    for (int i = 0; i < size_B; i++) {
      inputB[i] = ((double)i) / ((double)size_B);
    }
    /* for (int i = 0; i < hB; i++) { */
    /*   for (int j = 0; j < wB; j++) { */
    /*     if (i == j) */
    /*       inputB[i*wB+j] = 1.0; */
    /*     else */
    /*       inputB[i*wB+j] = 0.0; */
    /*   } */
    /* } */
    
    // Create reference result
    printf("Computing expected result\n");
    matrixMul_reference(expected, inputA, inputB, hA, wA, wB);

    /* print_matrix(inputA,wA,hA); */
    /* print_matrix(inputB,wB,hB); */
    /* print_matrix(expected,wC,hC); */

    printf("Computing result on GPU\n");
    mclDeviceData inputA_buf = mclDataToDevice(ctx, MCL_R, sizeof(double), size_A, inputA);
    mclDeviceData inputB_buf = mclDataToDevice(ctx, MCL_R, sizeof(double), size_B, inputB);
    mclDeviceData outbuf = mclAllocDevice(ctx, MCL_W, sizeof(double), size_C);

    // Also serves as warm-up
    matmul(ctx, matmulKernel, outbuf, inputA_buf, inputB_buf, wA, hA, wB);
    mclFinish(ctx);

    /* print_matrix(result,wC,hC); */

    printf("Checking results\n");
    double* out = (double*)mclMap(ctx, outbuf, CL_MAP_READ, size_C * sizeof(double));

    cl_int num_errors = 0;
    double epsilon = 0.0005;
    for (int i = 0; i < size_C; i++) {
      if (abs(out[i] - expected[i]) > epsilon) {
        num_errors++;
        if(num_errors > 10) {
          printf("Debug: more than 10 errors found. Stopping comparison.\n");
          break;
        } else {
          printf("Error: out[%d]!=expected[%d] (%f, %f)\n", i, i, out[i], expected[i]);
        }
      }
    }
    if (num_errors == 0) {
      printf("PASSED validation. No errors.\n");
    }
    mclUnmap(ctx, outbuf, out);

    if (num_errors == 0) {
      printf("Timing on %d executions\n", NUM_ITERATIONS);
      double secondstotal = 0.0;
      for (int i = 0; i < NUM_ITERATIONS; ++i) {
        unsigned long tdelta = matmul(ctx, matmulKernel, outbuf, inputA_buf, inputB_buf, wA, hA, wB);
        secondstotal += (((double)tdelta) / 1.0e9);
      }
      mclFinish(ctx);

      double seconds = secondstotal/(double)NUM_ITERATIONS;
      /* double mebibytes = (size_x * size_y * sizeof(int)) / (1024.0*1024.0); */
      /* double gebibytes = mebibytes / 1024.0; */

      double gebibytes = 0;
      double mebibytes = 0;
      printf("Stats for %s, Throughput = %.4f GiB/s, Time = %.5f s, Size = %.2f MiB, Workgroup = %u\n", "transpose",
             (2 * gebibytes) / seconds,
             seconds, mebibytes, BLOCK_DIM * BLOCK_DIM);

    }

    free(inputA);
    free(inputB);
    free(expected);
    mclReleaseDeviceData(&inputA_buf);
    mclReleaseDeviceData(&inputB_buf);
    mclReleaseDeviceData(&outbuf);
    mclReleaseKernel(matmulKernel);
    mclReleaseProgram(p);
    mclReleaseContext(&ctx);

    return 0;
}
