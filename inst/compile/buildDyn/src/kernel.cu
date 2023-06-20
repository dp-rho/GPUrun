#include "commands.h"
#include "cuda_headers.h"

/* Memory for Rvar structures stored in __constant__ access memory for faster execution */
__constant__ Rvar gpu_vars[MAX_VARS];

/* Memory for size of loop iterations stored in __constant__ access memory for faster execution */
__constant__ int gpu_iter_lens[MAX_ITERS];


/* Define functions available to kernel */

/*
 * Basic addition
 */

__device__ double add(double arg1, double arg2) {
  return arg1 + arg2;
}


/*
 * Basic subtraction
 */

__device__ double sub(double arg1, double arg2) {
  return arg1 - arg2;
}


/*
 * Basic multiplication
 */

__device__ double mul(double arg1, double arg2) {
  return arg1 * arg2;
}


/*
 * Basice division
 */

__device__ double dvs(double arg1, double arg2) {
  return arg1 / arg2;
}


/*
 * Kernel function ran on the GPU
 */

__global__
void kernel(int max_index, int grid_size)
{
  __shared__ double evals[THREADS_PER_BLOCK * EVALS_PER_THREAD];
  int data_index = blockDim.x * blockIdx.x + threadIdx.x;
  int thread_index = threadIdx.x;
  int _shared_mem_index = 0;
  int _eval_data_index = 0;
  cooperative_groups::grid_group grid = cooperative_groups::this_grid();

  // [[Kernel.start]]
  // Machine generated code
  // [[Kernel.end]]
}


/*
 * Top level function called from .cpp code to start the kernel
 */

void call_device() {

  /* TEMP */
  g_iter_lens[0] = 100000;
  g_iter_count = 1;
  int max_len = 100000;

  /* Copy the Rvars into __constant__ memory for faster execution in kernel */
  store_vars();
  
  /* Copy the iter lens into __constant__ memory for faster execution in kernel */
  store_iter_lens();

  /* Calculate the number of blocks needed and raise error if this exceeds  */
  /* the number of SMs, as we use shared memory on each block that requires */
  /* all available shared memory for a given SM                             */
  cudaDeviceProp deviceProp;
  int dev = 0;
  cudaGetDeviceProperties(&deviceProp, dev);
  int evals_per_block = THREADS_PER_BLOCK * EVALS_PER_THREAD;
  int blocks_per_grid = std::ceil(((float) max_len + THREADS_PER_BLOCK - 1) / evals_per_block);

  if (blocks_per_grid > deviceProp.multiProcessorCount) {
    printf("Error: Data too large for simultaneous execution on device\n");
  }

  int grid_size = blocks_per_grid * THREADS_PER_BLOCK;
  void* args[] = {&max_len, &grid_size};

  cudaLaunchCooperativeKernel((void*) kernel, blocks_per_grid, THREADS_PER_BLOCK, args);

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


/*
 * Copies variable info stored in CPU memory to __constant__ GPU memory
 */

void store_iter_lens() {
  cudaError_t err = cudaMemcpyToSymbol(gpu_iter_lens, g_iter_lens, sizeof(int) * g_iter_count);
  if (err != cudaSuccess) {
    printf("CUDA error while copying iteration lengths to __constant__ memory: %s\n", 
           cudaGetErrorString(err));
  }
  cudaDeviceSynchronize();

}
