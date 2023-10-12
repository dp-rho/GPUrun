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

/* Memory to be allocated on the GPU that is only used in scratch calculations  */
double* scratch_gpu_memory;


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
 * Inverse matrix using Gauss-Jordan elimination
 * Note: This function does not return a value directly and instead
 * updates values in the pointer argument working_result
 */

__device__ void inverse(Rvar matrix_arg, double* working_copy, double* working_result,
                        int grid_index, int evals_per_thread, int grid_size, int thread_index,
                        double* shared_mem_arr, cooperative_groups::grid_group grid) {

  /* Copy in matrix arg to the working copy, set working_result to identity matrix  */
  int data_index = grid_index;
  for (int i = 0; i < evals_per_thread; i++) {
    
    /* Check overflow */
    if (data_index >= matrix_arg.len) break;

    /* Copy data */
    working_copy[data_index] = matrix_arg.data[data_index];
    
    /* Fill in identity matrix  */
    if (data_index % matrix_arg.rdim == data_index / matrix_arg.rdim) {
      working_result[data_index] = 1;
    }
    else {
      working_result[data_index] = 0;
    }

    /* Update data index  */
    data_index += grid_size;
  }

  /* Sync grid before calculations begin  */
  grid.sync();

  /* Begin Gauss-Jordan elimination */
  for (int zero_col = 0; zero_col < matrix_arg.cdim; zero_col++) {
    int col_offset = matrix_arg.rdim * zero_col;    

    /* Ensure the diagnoal element of this column is not 0, if it is, ERROR  */
    /* TODO: diagonal elemenent */

    /* Sync all threads after row has been added  */
    grid.sync();

    /* Divide row of diagonal element by diagonal element */
    double divisor = working_copy[(zero_col * matrix_arg.rdim) + zero_col];
    if (grid_index < matrix_arg.cdim) {
      working_copy[(grid_index * matrix_arg.rdim) + zero_col] /= divisor;
    }
    else if (grid_index < 2 * matrix_arg.cdim) {
      working_result[(grid_index - matrix_arg.cdim) * matrix_arg.rdim + zero_col] /= divisor;
    }
    grid.sync();

    /* Save the zeroing column in shared mem in each block  */
    data_index = thread_index;
    while (data_index < matrix_arg.rdim) {
      shared_mem_arr[data_index] = working_copy[(zero_col * matrix_arg.rdim) + data_index];
      /* CHANGE TO BLOCK SIZE, make grid size and block size __constants__ for each kernel call */
      data_index += 256;
    }
    grid.sync();
   
    /* Zero out col using Ri <- Ri - Rj x aij for all i =/= j, with j the zeroing col  */ 
    data_index = grid_index;
    for (int i = 0; i < evals_per_thread; i++) {
      
      /* Check overflow */
      if (data_index >= matrix_arg.len) break;

      int col_index = data_index / matrix_arg.rdim;
      int row_index = data_index % matrix_arg.rdim;

      if (row_index != zero_col) {
        working_copy[data_index] -= (working_copy[col_index * matrix_arg.rdim + zero_col] *
                                     shared_mem_arr[row_index]);
        working_result[data_index] -= (working_result[col_index * matrix_arg.rdim + zero_col] *
                                       shared_mem_arr[row_index]);
      }

      data_index += grid_size;
    }
    grid.sync();
  }
  
}


/*
 * Kernel function ran on the GPU
 */

__global__
void kernel(int grid_size, double* scratch_gpu_memory)
{
  __shared__ double evals[THREADS_PER_BLOCK * MAX_EVALS_PER_THREAD];
  int grid_index = blockDim.x * blockIdx.x + threadIdx.x;
  int thread_index = threadIdx.x;
  int _shared_mem_index = 0;
  int _eval_data_index = 0;
  int _guard_len = 0;
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

  /* Initialize and copy intermediate evaluation variables  */
  initialize_int_evals();
  store_int_evals();
  
  /* Intialize and copy iter lens into __constant__ memory for faster execution in kernel */
  initialize_iter_lens();
  store_iter_lens();

  /* Intialize and copy expr lens into __constant__ memory for faster execution in kernel */
  initialize_expr_lens();
  store_expr_lens();
  int max_evals = *(std::max_element(g_evals_per_thread, g_evals_per_thread + g_expr_count));

  /* Calculate the number of evals needed per block and raise error if this exceeds */
  /* the maximum number of evaluations per block that has been pre calculated based */
  /* on a CUDA device with at least 48kb of __shared__ memory per SM. Note that the */
  /* evals per thread in each expression will vary as the goal is to maximize       */
  /* concurrency, thus larger expressions will require more evaluations per thread. */
  cudaDeviceProp deviceProp;
  int dev = 0;
  cudaGetDeviceProperties(&deviceProp, dev);
  int grid_size = deviceProp.multiProcessorCount * BLOCKS_PER_SM * THREADS_PER_BLOCK;

  if (max_evals > MAX_EVALS_PER_THREAD) {
    printf("Error: Data too large for simultaneous execution on device\n");
    return;
  }
  else {
    // CHECK HOW EFFICIENT 2 BLOCKS_PER_SM IS WITH SIMILAR SHARED MEMSIZE PER BLOCK
    printf("Launching %d blocks with %d threads per block\n",
           deviceProp.multiProcessorCount * BLOCKS_PER_SM, THREADS_PER_BLOCK);
    printf("Maximum concurrent evaluation of %d evals per thread\n", 
           max_evals);
  }

  scratch_gpu_memory = (double*) malloc_device(max_evals * deviceProp.multiProcessorCount * 
                                               BLOCKS_PER_SM * THREADS_PER_BLOCK * sizeof(double));

  void* args[] = {&grid_size, &scratch_gpu_memory};

  cudaLaunchCooperativeKernel((void*) kernel, deviceProp.multiProcessorCount * BLOCKS_PER_SM, 
                              THREADS_PER_BLOCK, args);

  /* Require GPU synchronize before CPU resume execution  */
  cudaDeviceSynchronize();

  /* Clean up memory from intermediate evaluations on GPU */
  free_int_evals();
  free_device(scratch_gpu_memory);
}


/*
 * Initializes the lengths of iteration loops using machine generated expressions,
 * this is called once at the start of each execution
 */

void initialize_iter_lens() {
  
  double access_mem[MAX_ITERS];
  
  /* The code below is updated by R code with expressions that are evaluated  */
  /* at each execution of the compiled commands to get the iteration length   */
  /* of each included loop                                                    */

  // [[Iter.mem::start]]
  /* Copy any memory accesses needed from GPU memory to CPU memory  */
  // [[Iter.mem::end]]


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
  double access_mem[MAX_EXPRS];

  // [[Expr.mem::start]]
  /* Copy any memory accesses needed from GPU memory to CPU memory  */
  // [[Expr.mem::end]]

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
  
  double access_mem[MAX_INT_VARS];

  // [[Int.mem::start]]
  /* Copy any memory accesses needed from GPU memory to CPU memory  */
  // [[Int.mem::end]]


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


