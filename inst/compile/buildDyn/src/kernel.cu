#include "commands.h"
#include "cuda_headers.h"

/* Memory for Rvar structures stored in __constant__ access memory for faster execution */
__constant__ Rvar gpu_vars[MAX_VARS];


/*
 * Kernel function ran on the GPU
 */

__global__
void kernel()
{
  // [[Start.kernel]]
  // Machine generated code
  // [[End.kernel]]
}


/*
 * Top level function called from .cpp code to start the kernel
 */

void call_device() {
  
  /* Copy the Rvars into __constant__ memory for faster execution in kernel */
  store_vars();

  // Run kernel on the GPU
  kernel<<<1, 1>>>();

  // Wait for GPU to finish before accessing on host
  cudaDeviceSynchronize();
  
}


/*
 * Copies variable info stored in CPU memory to __constant__ GPU memory
 */

void store_vars() {
  cudaError_t err = cudaMemcpyToSymbol(gpu_vars, g_vars, sizeof(Rvar) * g_var_count);
  if (err != cudaSuccess) {
    printf("CUDA error while copying Rvars to __constant__ memory: %s\n", cudaGetErrorString(err));
  }
  cudaDeviceSynchronize();
}

