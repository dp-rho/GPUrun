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
void kernel(int max_index, int grid_size, int evals_per_thread)
{
  __shared__ double evals[THREADS_PER_BLOCK * MAX_EVALS_PER_THREAD];
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

  /* Calculate the number of evals needed per block and raise error if this exceeds */
  /* the maximum number of evaluations per block that has been pre calculated based */
  /* on a CUDA device with 48kb of __shared__ memory per block                      */
  cudaDeviceProp deviceProp;
  int dev = 0;
  cudaGetDeviceProperties(&deviceProp, dev);
  int grid_size = THREADS_PER_BLOCK * deviceProp.multiProcessorCount * BLOCKS_PER_SM;
  int evals_per_thread = std::ceil((float) max_len / grid_size);
  
  if (evals_per_thread > MAX_EVALS_PER_THREAD) {
    printf("Error: Data too large for simultaneous execution on device\n");
  }
  else {
    printf("Launching %d blocks with %d threads per block and %d evals per thread\n",
           deviceProp.multiProcessorCount * BLOCKS_PER_SM, THREADS_PER_BLOCK, evals_per_thread);
  }

  void* args[] = {&max_len, &grid_size, &evals_per_thread};

  cudaLaunchCooperativeKernel((void*) kernel, deviceProp.multiProcessorCount * BLOCKS_PER_SM, THREADS_PER_BLOCK, 
                              args);

  // Wait for GPU to finish before accessing on host
  cudaDeviceSynchronize();
  
}

/*
 * Initializes the lengths of iteration loops using machine generated expressions,
 * this is called once at the start of each execution
 */

void initialize_iter_lens() {

  /* The code below is updated by R code with expressions that are evaluated  */
  /* at each execution of the compiled commands to get the iteration length   */
  /* of each included loop                                                    */
  // [[Iter.lens::start]]
  g_iter_lens[0] = 0;
  g_iter_lens[1] = 0;
  g_iter_lens[2] = 0;
  g_iter_lens[3] = 0;
  g_iter_lens[4] = 0;
  g_iter_lens[5] = 0;
  g_iter_lens[6] = 0;
  g_iter_lens[7] = 0;
  g_iter_lens[8] = 0;
  g_iter_lens[9] = 0;

  g_iter_count = /* R::g_loop_count */;
  // [[Iter.lens::end]]
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
