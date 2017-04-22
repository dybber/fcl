#include <thrust/host_vector.h>
#include <thrust/device_vector.h>
#include <thrust/generate.h>
#include <thrust/reduce.h>
#include <thrust/functional.h>
#include <algorithm>
#include <cstdlib>

int main(void)
{
  unsigned int size = 4096*4096;
  thrust::host_vector<int32_t> input_host(size);
  for(int i=0; i < size; i++){
    input_host[i] = i;
  }
  //std::generate(input_host.begin(), input_host.end(), rand);
  for(int i=0; i < 100; i++){
    printf("%d,", input_host[i]);
  }
  printf("\n");

  
  // transfer to device and compute sum
  thrust::device_vector<int32_t> input_device = input_host;
  thrust::device_vector<int32_t> output_device(size);

  thrust::plus<int32_t> binary_op;
  thrust::exclusive_scan(input_device.begin(), input_device.end(), output_device.begin(), 0, binary_op); // in-place scan

  thrust::host_vector<int32_t> output_host = output_device;
  for(int i=0; i < 100; i++){
    printf("%d,", output_host[i]);
  }
  printf("\n");
  
  cudaEvent_t start, stop;
  cudaEventCreate(&start);
  cudaEventCreate(&stop);
  cudaEventRecord(start, 0);
  for(int i=0; i < 100; i++){
    //int x = thrust::reduce(input_device.begin(), input_device.end(), 0, binary_op);
    thrust::exclusive_scan(input_device.begin(), input_device.end(), output_device.begin(), 0, binary_op); // in-place scan
  }
  cudaEventRecord(stop, 0);
  cudaEventSynchronize(stop);
  float elapsedTime; 
  cudaEventElapsedTime(&elapsedTime , start, stop);
  printf("Avg. time is %f ms\n", elapsedTime/100);
  
  return 0;
}
