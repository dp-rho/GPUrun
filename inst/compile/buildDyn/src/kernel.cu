#include "commands.h"
#include "cuda_headers.h"

/* Memory for Rvar structures stored in __constant__ access memory for faster execution */
__constant__ Rvar gpu_vars[MAX_VARS];

/* Memory for Rvar structures that store intermediate evaluations in __constant__ access memory */
__constant__ Rvar gpu_int_evals[MAX_INT_VARS];

/* Memory for size of loop iterations stored in __constant__ access memory for faster execution */
__constant__ int gpu_iter_lens[MAX_ITERS];

/* Memory for size of expressions stored in __constant__ access memory for faster execution */
__constant__ int gpu_evals_per_thread[MAX_EXPRS];


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
 * Range operator, i.e., from 1:10, return 1, 2, 3, ...
 */

__device__ double range(double arg1, double arg2, int data_index) {
  int sign = (arg2 > arg1) ? 1 : -1;
  int len = std::floor(abs(arg2 - arg1) + 1);
  return arg1 + (sign * (data_index % len));
}


/*
 * Matrix multiplication, implementation is naive with no use of shared memory
 */

__device__ double mat_mul(Rvar arg1, Rvar arg2, int data_index) {

  /* Check if evaluation index is out of bounds of return matrix  */
  if (data_index > arg1.rdim * arg2.cdim) return 0;

  /* Identify the row and column index of the element being calculated  */
  int row_index = data_index % arg1.rdim;
  int col_index = data_index / arg1.rdim;

  /* The evaluated result of the input index  */
  double result = 0;

  /* Loop through the selected row and column of arg1 and arg2 and calculated dot product */
  for (int i = 0; i < arg1.cdim; i++) {
    result += (arg1.data[(i * arg1.rdim) + row_index] * 
               arg2.data[(col_index * arg2.rdim) + i]);
  }

  return result;
}


/*
 * Transpose matrix
 */

__device__ double transpose(Rvar arg, int data_index) {
 
  /* Check if evaluation index is out of bounds of return matrix  */
  if (data_index > arg.rdim * arg.cdim) return 0;

  /* Identify the row and column index of the element being calculated  */
  int row_index = data_index % arg.rdim;
  int col_index = data_index / arg.rdim;

  /* Return the transposed index of the argument matrix */
  return arg.data[(arg.rdim * row_index) + col_index];
 
}


/*
 * Kernel function ran on the GPU
 */

__global__
void kernel(int grid_size)
{
  __shared__ double evals[THREADS_PER_BLOCK * MAX_EVALS_PER_THREAD];
  int grid_index = blockDim.x * blockIdx.x + threadIdx.x;
  int thread_index = threadIdx.x;
  int _shared_mem_index = 0;
  int _eval_data_index = 0;
  cooperative_groups::grid_group grid = cooperative_groups::this_grid();

  // [[Kernel::start]]
  // Machine generated code
  // [[Kernel::end]]
}


/*
 * Top level function called from .cpp code to start the kernel
 */

void call_device() {

  /* Copy the Rvars into __constant__ memory for faster execution in kernel */
  store_vars();

  /* Initialize and copy the intermediate evaluation variables  */
  initialize_int_evals();
  store_int_evals();
  
  /* Intialize and copy the iter lens into __constant__ memory for faster execution in kernel */
  initialize_iter_lens();
  store_iter_lens();

  /* Intialize and copy the expr lens into __constant__ memory for faster execution in kernel */
  initialize_expr_lens();
  store_expr_lens();
  int max_len = *(std::max_element(g_evals_per_thread, g_evals_per_thread + g_expr_count));

  /* Calculate the number of evals needed per block and raise error if this exceeds */
  /* the maximum number of evaluations per block that has been pre calculated based */
  /* on a CUDA device with at least 48kb of __shared__ memory per SM. Note that the */
  /* evals per thread in each expression will vary as the goal is to maximize       */
  /* concurrency, thus larger expressions will require more evaluations per thread. */
  cudaDeviceProp deviceProp;
  int dev = 0;
  cudaGetDeviceProperties(&deviceProp, dev);
  int grid_size = THREADS_PER_BLOCK * deviceProp.multiProcessorCount * BLOCKS_PER_SM;
  int evals_per_thread = std::ceil((float) max_len / grid_size);
  
  if (evals_per_thread > MAX_EVALS_PER_THREAD) {
    printf("Error: Data too large for simultaneous execution on device\n");
    return;
  }
  else {
    // CHECK HOW EFFICIENT 2 BLOCKS_PER_SM IS WITH SIMILAR SHARED MEMSIZE PER BLOCK
    printf("Launching %d blocks with %d threads per block\n",
           deviceProp.multiProcessorCount * BLOCKS_PER_SM, THREADS_PER_BLOCK);
  }

  void* args[] = {&grid_size};

  cudaLaunchCooperativeKernel((void*) kernel, deviceProp.multiProcessorCount * BLOCKS_PER_SM, 
                              THREADS_PER_BLOCK, args);

  /* Require GPU synchronize before CPU resume execution  */
  cudaDeviceSynchronize();

  /* Clean up memory from intermediate evaluations on GPU */
  free_int_evals();
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
  g_iter_lens[/*x*/] = /* parsed expr len */
  g_iter_count = /* R::g_loop_count */;
  // [[Iter.lens::end]]
}


/*
 * Initializes the lengths of iteration loops using machine generated expressions,
 * this is called once at the start of each execution
 */

void initialize_expr_lens() {

  /* Retrieve the grid size to allow calculation of expr specific evals per thread  */
  cudaDeviceProp deviceProp;
  int dev = 0;
  cudaGetDeviceProperties(&deviceProp, dev);
  int grid_size = THREADS_PER_BLOCK * deviceProp.multiProcessorCount * BLOCKS_PER_SM;
  int expr_len = 0;

  /* The code below is updated by R code with expressions that are evaluated  */
  /* at each execution of the compiled commands to get the expression length  */
  /* of each included expression                                              */

  // [[Expr.lens::start]]
  expr_len = /* parsed expr len */;
  g_evals_per_thread[/*x*/] = std::ceil((float) expr_len / grid_size);
  g_expr_count = /* R::g_expr_count */;
  // [[Expr.lens::end]]
}


/*
 * Initializes the lengths and dimensions of intermediate evaluation variables
 * used to store evaluations of matrix function arguments
 */

void initialize_int_evals() {
  
  /* Used to intitialize len and data fields  */
  int len = 0;  

  // [[Int.evals::start]]
  len = /* parsed expr len */
  g_int_evals[/*x*/] = {
    .data = (double*) malloc_device(sizeof(double) * len),
    .len = len,
    .rdim = /* parsed expr rdim */,
    .cdim = /* parsed expr cdim */
  };
  g_int_eval_count = /* R::g_int_eval_count */;
  // [[Int.evals::end]]
}


/*
 * Frees the allocated memory associated with intermediate evaluations
 */

void free_int_evals() {
  for (int i = 0; i < g_int_eval_count; i++) {
    free_device(g_int_evals[i].data);
  }
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
 * Copies iteration loop info stored in CPU memory to __constant__ GPU memory
 */

void store_iter_lens() {
  cudaError_t err = cudaMemcpyToSymbol(gpu_iter_lens, g_iter_lens, sizeof(int) * g_iter_count);
  if (err != cudaSuccess) {
    printf("CUDA error while copying iteration lengths to __constant__ memory: %s\n", 
           cudaGetErrorString(err));
  }
  cudaDeviceSynchronize();
}


/*
 * Copies expression length info stored in CPU memory to __constant__ GPU memory
 */

void store_expr_lens() {
  cudaError_t err = cudaMemcpyToSymbol(gpu_evals_per_thread, g_evals_per_thread, 
                                       sizeof(int) * g_expr_count);
  if (err != cudaSuccess) {
    printf("CUDA error while copying expression lengths to __constant__ memory: %s\n",
           cudaGetErrorString(err));
  }
  cudaDeviceSynchronize();
}


/*
 * Copies intermediate Rvar structures from CPU memory to __constant__ memory
 */

void store_int_evals() {
  cudaError_t err = cudaMemcpyToSymbol(gpu_int_evals, g_int_evals, sizeof(Rvar) * g_int_eval_count);
  if (err != cudaSuccess) {
    printf("CUDA error while copying intermediate evaluations to __constant__ memory: %s\n", 
           cudaGetErrorString(err));
  }
  cudaDeviceSynchronize();
}


