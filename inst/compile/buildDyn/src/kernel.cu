#include "commands.h"
#include "cuda_headers.h"

/* Memory for Rvar structures stored in __constant__ access memory for faster execution */
__constant__ Rvar gpu_vars[MAX_VARS];


/*
 * Kernel function ran on the GPU
 */

__global__
void add(double* out, int var_count)
{
  double sum = 0;
  for (int i = 0; i < var_count; i++) {
    sum += gpu_vars[i].data[0];
    gpu_vars[i].data[0]++;
  }

  *out = sum;
}


/*
 * Top level function called from .cpp code to start the kernel
 */

void call_device() {
  
  /* Copy the Rvars into __constant__ memory for faster execution in kernel */
  store_vars();

  double* result;
  cudaMallocManaged(&result, sizeof(double));

  // Run kernel on the GPU
  add<<<1, 1>>>(result, g_var_count);

  // Wait for GPU to finish before accessing on host
  cudaDeviceSynchronize();

  printf("Result is: %f\n", *result);

  cudaFree(result);

}


/*
 * Copies variable info stored in CPU memory to __constant__ GPU memory
 */

void store_vars() {
  printf("g_var_count: %d\n", g_var_count);
  cudaError_t err = cudaMemcpyToSymbol(gpu_vars, g_vars, sizeof(Rvar) * g_var_count);
  if (err != cudaSuccess) {
    printf("CUDA error while copying Rvars to __constant__ memory: %s\n", cudaGetErrorString(err));
  }

  cudaDeviceSynchronize();
}

