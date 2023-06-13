#include "commands.h"
#include "cuda_headers.h"

/* Memory for Rvar structures stored in __constant__ access memory for faster execution */
__constant__ Rvar gpu_vars[MAX_VARS];


/*
 * Forces synchronization of all threads in all blocks using 
 * CUDA's built in cooperative_groups functionality.
 * Note this requires all blocks be the same size, however,
 * this is always the case in this program.
 */

__device__ void sync_blocks() {
  /* Do we need to sync in block first? */
  cooperative_groups::thread_block block = cooperative_groups::this_thread_block();
  block.sync();
  auto group = cooperative_groups::coalesced_threads();
  group.sync();
}


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
void kernel(int max_index)
{
  int thread_index = blockDim.x * blockIdx.x + threadIdx.x;
  // [[Kernel.start]]
  // Machine generated code
  // [[Kernel.end]]
}


/*
 * Top level function called from .cpp code to start the kernel
 */

void call_device() {
  
  /* Copy the Rvars into __constant__ memory for faster execution in kernel */
  store_vars();

  /* TEMP */
  int max_len = 50000;

  /* Number of blocks to use in device call */
  int blocks_per_grid = (max_len + THREADS_PER_BLOCK - 1) / THREADS_PER_BLOCK;

  // Run kernel on the GPU
  kernel<<<blocks_per_grid, THREADS_PER_BLOCK>>>(max_len);

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

