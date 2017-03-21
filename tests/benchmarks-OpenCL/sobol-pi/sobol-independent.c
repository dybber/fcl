#include <stdint.h>
#include <stdio.h>
#include <sys/time.h>
#include <mcl.h>
#include "sobol.h"
#include "DirectionVectors.h"

#define BLOCK_DIM 16
#define NUM_ITERATIONS 100

#define timediff(old, new) (((double)new.tv_sec + 1.0e-6 * (double)new.tv_usec) \
                            - ((double)old.tv_sec + 1.0e-6 * (double)old.tv_usec))

LoopROScalars defaultScalars =
  { .num_mcits = 1048576,
    .sobol_bits = 30,
    .sobol_dim = 15,
    .sobol_count_ini = 0,
    .sob_norm_fact = 1.0 / (1<<30),
    .log_chunk = 8,
    .chunk = 256,
    .logBLOCK = 7,
    .BLOCK = 1 << 7
  };

void sobol(mclContext ctx, cl_kernel kernel,
           mclDeviceData ro_scal, mclDeviceData sobol_v_dir, mclDeviceData output,
           int globalWorkSize,
           int localWorkSize) {
    uint8_t dummy = 0;
    mclSetKernelArg(kernel, 0, sizeof(LoopROScalars), &ro_scal.data);
    mclSetKernelArg(kernel, 1, sizeof(uint8_t), &dummy); // Not used in the independent case
    mclSetKernelArg(kernel, 2, sizeof(cl_mem), &sobol_v_dir.data);
    mclSetKernelArg(kernel, 3, sizeof(cl_mem), &output.data);

    mclInvokeKernel(ctx, kernel, globalWorkSize, localWorkSize);
}

uint32_t getWorkSize(const uint32_t num_iter, const uint32_t chunk, const uint32_t block) {
	uint32_t tmp_rem = num_iter % chunk;
	uint32_t res     = (tmp_rem == 0) ? 
                         (num_iter / chunk)      : 
                        ((num_iter / chunk) + 1) ;

	if(res % block == 0) return res;

	res = (res / block) * block + block;
	return res;
}

void test_sobol(mclContext ctx, cl_program p) {
    cl_kernel sobolKernel = mclCreateKernel(p, "mlfi_genmatrix_uniform2");

    LoopROScalars ro_scal = defaultScalars;

    uint32_t lenDirVec = ro_scal.sobol_dim * ro_scal.sobol_bits;

    /* Move data to GPU */
    mclDeviceData scal_buf = mclDataToDevice(ctx, MCL_R, 1, sizeof(LoopROScalars), &ro_scal);
    mclDeviceData dirVec = mclDataToDevice(ctx, MCL_R, lenDirVec, sizeof(cl_int), sobolDirVec);
    mclDeviceData outbuf = mclAllocDevice(ctx, MCL_RW, ro_scal.num_mcits, sizeof(cl_double));

    int globalWorkSize = getWorkSize(ro_scal.num_mcits, ro_scal.chunk, ro_scal.BLOCK);
    printf("blocks: %d\n", globalWorkSize);
    printf("wgsize: %d\n", ro_scal.BLOCK);


    // Warm-up and verify
    sobol(ctx, sobolKernel, scal_buf, dirVec, outbuf, globalWorkSize, ro_scal.BLOCK);
    mclFinish(ctx);

    cl_double *out = (cl_double*)calloc(ro_scal.num_mcits, sizeof(cl_double));
    cl_int status = clEnqueueReadBuffer(  ctx.command_queue, outbuf.data, CL_TRUE, 0,
                                          ro_scal.num_mcits * sizeof(cl_double),
                                          (void*)out, 0, NULL, NULL);

    if(status == CL_SUCCESS) {
      printf("SUCCESS\n");
    }

    /* cl_double* out = (cl_double*)mclMap(ctx, outbuf, CL_MAP_READ, ro_scal.num_mcits * sizeof(cl_double)); */

    for (int i = 0; i < 50; i++) {
      printf("%f\n", out[i]);
    }


    /* Compare with expected output */
    /* cl_int num_errors = 0; */
    /* for (int i = 0; i < ro_scal.num_mcits; i++) { */
    /*   if (out[i] != expected[i]) { */
    /*     num_errors++; */
    /*     if(num_errors > 10) { */
    /*       printf("Debug: more than 10 errors found. Stopping comparison.\n"); */
    /*       break; */
    /*     } else { */
    /*       printf("Error: out[%d]!=expected[%d] (%d, %d)\n", i, i, out[i], expected[i]); */
    /*     } */
    /*   } */
    /* } */
    /* mclUnmap(ctx, outbuf, out); */

    /* if (num_errors == 0) { */
    /*   printf("Timing on %d executions\n", NUM_ITERATIONS); */
    /*   struct timeval begin, end; */
    /*   gettimeofday(&begin, NULL); */
    /*   for (int i = 0; i < NUM_ITERATIONS; ++i) { */
    /*       sobol(ctx, sobolKernel, outbuf, buf, 0, size_x, size_y); */
    /*   } */
    /*   mclFinish(ctx); */
    /*   gettimeofday(&end, NULL); */

    /*   double time = (timediff(begin, end))/(double)NUM_ITERATIONS; */

    /*   printf("Stats for %s, Throughput = %.4f GB/s, Time = %.5f s, Size = %u fp32 elements, Workgroup = %u\n", kernelName, */
    /*          (1.0e-9 * (double)(size_x * size_y * sizeof(float))/time), */
    /*          time, (size_x * size_y), BLOCK_DIM * BLOCK_DIM); */

    /* } */

    mclReleaseDeviceData(&dirVec);
    mclReleaseDeviceData(&outbuf);
    mclReleaseKernel(sobolKernel);
}

int main () {
  mclContext ctx = mclInitialize(0);
  cl_program p = mclBuildProgram(ctx, "cosmin-independent.cl");

  test_sobol(ctx, p);

  mclReleaseProgram(p);
  mclReleaseContext(&ctx);
}
