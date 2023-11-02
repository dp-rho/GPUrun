#include "commands.h"
#include "cuda_headers.h"

/* Memory for Rvar structures stored in __constant__ access memory for faster execution */
__constant__ Rvar gpu_vars[MAX_VARS];

/* Memory for Rvars that stored intermediate evaluations in __constant__ access memory */
__constant__ Rvar gpu_int_evals[MAX_INT_VARS];

/* Memory for size of loop iterations in __constant__ access memory for faster execution */
__constant__ int gpu_iter_lens[MAX_ITERS];

/* Memory for size of expressions stored in __constant__ access memory for faster execution */
__constant__ int gpu_evals_per_thread[MAX_EXPRS];

/* Memory to be allocated on the GPU that is only used in multi purpose intermediate  */
/* storage, the most common is intermediate storage after an expression evaluation    */
/* before a global variable is updated with the evaluated values                      */
double* scratch_gpu_memory;

/* Memory for linear algebra functions  */
double* gpu_Q;
double* gpu_tridiagonal;
double* gpu_eigvecs;
double* gpu_eigvals;


/* Define functions available to kernel */

/* Random number functions  */

/*
 * Generate a uniform random variable between a and b
 */

__device__ double runif_device(double a, double b, curandState_t* random_state) {
  double rs = curand_uniform_double(random_state);
  return ((b - a) * rs) + a;
}


/*
 * Uniform density
 */

__device__ double dunif_device(double a, double b) {
  return 1 / (b - a);
}


/*
 * Generate an exponential random variable with in scale
 * NOTE: Taken from rexp source code
 */

__device__ double rexp_device(double scale, curandState_t* random_state) {
  
  /* q[k-1] = sum(log(2)^k / k!)  k=1,..,n, */
  /* The highest n (here 16) is determined by q[n-1] = 1.0 */
  /* within standard precision */
  const static double q[] =
  {
    0.6931471805599453,
	  0.9333736875190459,
	  0.9888777961838675,
	  0.9984959252914960,
	  0.9998292811061389,
	  0.9999833164100727,
	  0.9999985691438767,
	  0.9999998906925558,
	  0.9999999924734159,
	  0.9999999995283275,
	  0.9999999999728814,
	  0.9999999999985598,
	  0.9999999999999289,
	  0.9999999999999968,
	  0.9999999999999999,
	  1.0000000000000000
  };

  double a = 0.;
  double u = curand_uniform_double(random_state);
  while(u <= 0. || u >= 1.) u = curand_uniform_double(random_state);
  for (;;) {
    u += u;
    if (u > 1.) break;
    a += q[0];
  }
  u -= 1.;

  if (u <= q[0]) return a + u;
    
  int i = 0;
  double ustar = curand_uniform_double(random_state); 
  double umin = ustar;
  do {
    ustar = curand_uniform_double(random_state);
    if (umin > ustar)
      umin = ustar;
    i++;
  } while (u > q[i]);

  return (a + umin * q[0]) * scale;
}


/*
 * Exponential density
 */

__device__ double dexp_device(double x, double scale) {
  return (1 / scale) * exp(-x / scale);
}

/*
 * Generate a normal random variable with mean mu and standard deviation sd
 */

__device__ double rnorm_device(double mean, double sd, curandState_t* random_state) {
  double rs = curand_normal_double(random_state);
  return (rs * sd) + mean;
}


/*
 * Normal density
 */

__device__ double dnorm_device(double x, double mean, double sd) {
  x = (x - mean) / sd;
  return M_1_SQRT_2PI * exp(-0.5 * x * x) / sd;
} 


/*
 * Generate a truncated normal variable with mean and standard deviation sd
 * NOTE: Implementation taken directly from Rtruncnorm package
 */


/* Exponential rejection sampling (a,inf) */
__device__ double ers_a_inf(double a, curandState_t* random_state) {
  const double ainv = 1.0 / a;
  double x, rho;
  do {
    x = rexp_device(ainv, random_state) + a;
    rho = exp(-0.5 * pow((x - a), 2));
  } while (runif_device(0, 1, random_state) > rho);
  return x;
}

/* Exponential rejection sampling (a,b) */
__device__ double ers_a_b(double a, double b, curandState_t* random_state) {
  const double ainv = 1.0 / a;
  double x, rho;
  do {
    x = rexp_device(ainv, random_state) + a;
    rho = exp(-0.5 * pow((x - a), 2));
  } while (runif_device(0, 1, random_state) > rho || x > b);
  return x;
}

/* Normal rejection sampling (a,b) */
__device__ double nrs_a_b(double a, double b, curandState_t* random_state) {
  double x = -DBL_MAX;
  while (x < a || x > b) {
    x = rnorm_device(0, 1, random_state);
  }
  return x;
}

/* Normal rejection sampling (a,inf) */
__device__ double nrs_a_inf(double a, curandState_t* random_state) {
  double x = -DBL_MAX;
  while (x < a) {
    x = rnorm_device(0, 1, random_state);
  }
  return x;
}

/* Half-normal rejection sampling */
__device__ double hnrs_a_b(double a, double b, curandState_t* random_state) {
  double x = a - 1.0;
  while (x < a || x > b) {
    x = rnorm_device(0, 1, random_state);
    x = fabs(x);
  }
  return x;
}

/* Uniform rejection sampling */
__device__ double urs_a_b(double a, double b, curandState_t* random_state) {
  
  const double phi_a = dnorm_device(a, 0, 1);
  double x = 0.0, u = 0.0;

  /* Upper bound of normal density on [a, b] */
  const double ub = a < 0 && b > 0 ? M_1_SQRT_2PI : phi_a;
  do {
    x = runif_device(a, b, random_state);
  } while (runif_device(0, 1, random_state) * ub > dnorm_device(x, 0, 1));
  return x;
}

/* Truncated on the left  */
__device__ double rtruncnorm_left(double a, double mean, double sd, 
                                  curandState_t* random_state) {
  const double alpha = (a - mean) / sd;
  if (alpha < T4) {
    return mean + sd * nrs_a_inf(alpha, random_state);
  } else {
    return mean + sd * ers_a_inf(alpha, random_state);
  }
}

/* Truncated on the right */
__device__ double rtruncnorm_right(double b, double mean, double sd, 
                                   curandState_t* random_state) {
  const double beta = (b - mean) / sd;
  return mean - sd * rtruncnorm_left(-beta, 0.0, 1.0, random_state);
}

/* General case */
__device__ double rtruncnorm_general(double a, double b, double mean, double sd,
                                     curandState_t* random_state) {
  const double alpha = (a - mean) / sd;
  const double beta = (b - mean) / sd;
  const double phi_a = dnorm_device(alpha, 0.0, 1.0);
  const double phi_b = dnorm_device(beta, 0.0, 1.0);
  if (alpha <= 0 && 0 <= beta) { 
    if (phi_a <= T2 || phi_b <= T1) {  
      return mean + sd * nrs_a_b(alpha, beta, random_state);
    } else { 
      return mean + sd * urs_a_b(alpha, beta, random_state);
    }
  } else if (alpha > 0) { 
    if (phi_a / phi_b <= T1) {
      return mean + sd * urs_a_b(alpha, beta, random_state);
    } else {
      if (alpha < T2) {
        return mean + sd * hnrs_a_b(alpha, beta, random_state);
      } else {
        return mean + sd * ers_a_b(alpha, beta, random_state);
      }
    }
  } else {
    if (phi_b / phi_a <= T2) {
      return mean - sd * urs_a_b(-beta, -alpha, random_state);
    } else {
      if (beta > -T3) {
        return mean - sd * hnrs_a_b(-beta, -alpha, random_state);
      } else {
        return mean - sd * ers_a_b(-beta, -alpha, random_state);
      }
    }
  }
  return 0;
}

/*
 * Generate a random truncated normal variable with potential truncation points a, b
 */

__device__ double rtruncnorm_device(double a, double b, double mean, double sd,
                                    curandState_t* random_state) {

  const int a_finite = (a == -DBL_MAX) ? 0 : 1;
  const int b_finite = (b == DBL_MAX) ? 0 : 1;

  if (a_finite && b_finite) {
    return rtruncnorm_general(a, b, mean, sd, random_state);
  } else if (!a_finite && b_finite) {
    return rtruncnorm_right(b, mean, sd, random_state);
  } else if (a_finite && !b_finite) {
    return rtruncnorm_left(a, mean, sd, random_state);
  } else if (!a_finite && !b_finite) {
    return rnorm_device(mean, sd, random_state);
  } 

  return 0;
}


/* Basic vector math functions  */

/*
 * Basic addition
 */

__host__ __device__ double add(double arg1, double arg2) {
  return arg1 + arg2;
}

/*
 * Basic subtraction
 */

__host__ __device__ double sub(double arg1, double arg2) {
  return arg1 - arg2;
}


/*
 * Basic multiplication
 */

__host__ __device__ double mul(double arg1, double arg2) {
  return arg1 * arg2;
}


/*
 * Basice division
 */

__host__ __device__ double dvs(double arg1, double arg2) {
  return arg1 / arg2;
}


/*
 * Range operator, i.e., from 1:10, return 1, 2, 3, ...
 */

__host__ __device__ double range(double arg1, double arg2, int data_index) {
  int sign = (arg2 > arg1) ? 1 : -1;
  int len = floor(abs(arg2 - arg1) + 1);
  return arg1 + (sign * (data_index % len));
}


/*
 * Matrix multiplication, implementation is naive with no use of shared memory
 */

__device__ double mat_mul(Rvar arg1, Rvar arg2, int data_index) {

  /* Check if evaluation index is out of bounds of return matrix  */
  if (data_index >= arg1.rdim * arg2.cdim) return 0;

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
  if (data_index >= arg.rdim * arg.cdim) return 0;

  /* Identify the row and column index of the element being calculated  */
  int row_index = data_index % arg.cdim;
  int col_index = data_index / arg.cdim;

  /* Return the transposed index of the argument matrix */
  return arg.data[(arg.rdim * row_index) + col_index];
 
}


/*
 * Inverse matrix using Gauss-Jordan elimination
 * Note: This function does not return a value directly and instead
 * updates values in the pointer argument working_result
 */

__device__ void inverse(Rvar matrix_arg, double* working_copy, 
                        double* working_result,
                        int grid_index, int evals_per_thread, int grid_size, 
                        int thread_index, double* shared_mem_arr, 
                        cooperative_groups::grid_group grid) {

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
    /* NOTE: Currently does not support matricies with more than SMs * 256 columns */
    double divisor = working_copy[(zero_col * matrix_arg.rdim) + zero_col];
    if (grid_index < matrix_arg.cdim) {
      working_copy[(grid_index * matrix_arg.rdim) + zero_col] /= divisor;
    }
    else if (grid_index < 2 * matrix_arg.cdim) {
      working_result[(grid_index - matrix_arg.cdim) * matrix_arg.rdim + zero_col] /= divisor;
    }
    grid.sync();

    /* Save the zeroing column in shared mem in each block  */
    /* NOTE: Not supported for matrices with more than 256 * MAX_EVALS_PER_THREAD rows  */
    data_index = thread_index;
    while (data_index < matrix_arg.rdim) {
      shared_mem_arr[data_index] = working_copy[(zero_col * matrix_arg.rdim) + data_index];
      data_index += THREADS_PER_BLOCK;
    }
    __syncthreads();
   
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
 * Reduces symmetric PSD matrix argument to tridiagonal form and also stores Q,
 * with Q = P(n-2) %*% P(n-3) ... P(1) where P is the householder matrix used
 * to reduce input matrix at each iteration.
 * NOTE: No output is produced by this function, the global variables of 
 * gpu_Q and gpu_tridiagonal are updated.
 */

__device__ void householder_reduction(Rvar matrix_arg, double* gpu_Q,
                                      double* gpu_tridiagonal, double* shared_arr,  
                                      double* eval_memory, double* linalg_vec, int grid_size,
                                      int grid_index, int thread_index, int evals_per_thread,
                                      cooperative_groups::grid_group grid) {
  
  int row_index = grid_index % matrix_arg.rdim;
  int col_index = grid_index / matrix_arg.cdim;
  int data_index = grid_index;
  __shared__ double scaler1;
  __shared__ double scaler2;

  /* First initialize global memory for Q matrix and tridiagonal matrix */
  for (int i = 0; i < evals_per_thread; i++) {
    
    /* Check potential overflow */
    if (data_index > matrix_arg.len) break;

    /* Initialize matrix that will store tridiagonal results  */
    gpu_tridiagonal[data_index] = matrix_arg.data[data_index];

    /* Initialize Q to identity matrix */
    if (row_index == col_index) {
      gpu_Q[data_index] = 1;
    }
    else {
      gpu_Q[data_index] = 0;
    }

    data_index += grid_size;
    row_index = data_index % matrix_arg.rdim;
    col_index = data_index / matrix_arg.cdim;
  }

  /* Sync all threads after initialzation */
  grid.sync();

  /* Loop n-2 times to transform to tridiagonal form using householder matrices */
  for (int i = 0; i < (matrix_arg.rdim - 2); i++ ) {

    /* vectors x, u, p and q are all created in shared memory across all SMs  */
     
    /* create vector x as bottom n-(i+1) elements of current matrix */
    data_index = thread_index;
    while (data_index < matrix_arg.rdim) {
      if (data_index < (i + 1)) {
        shared_arr[data_index] = 0;
      }
      else {
        shared_arr[data_index] = gpu_tridiagonal[(i * matrix_arg.rdim) + data_index];
      }
      data_index += THREADS_PER_BLOCK;
    }
    __syncthreads();
    
    /* create vector u <- x + (sign) * norm(x) * e1, u overwrites x in shared_arr */
    /* This is not a parallel operation, as norm is sequential and only one index */
    /* is updated in the vector x, thus only the first thread of each block used  */
    if (thread_index == DEFAULT_DATA_INDEX) {
 
      /* determine sign for u = x + (sign) * e1 * norm(x) */
      int sign = (gpu_tridiagonal[matrix_arg.len - 1] > 0) ? 1 : -1;
 
      scaler1 = 0;
      for (int j = (i + 1); j < matrix_arg.rdim; j++) {
        scaler1 += (shared_arr[j] * shared_arr[j]);
      }

      /* Get norm of x  */
      double l1_norm = sqrt(scaler1);

      /* subtract the squared element that will be updated to create u  */
      scaler1 -= (shared_arr[i + 1] * shared_arr[i + 1]);

      /* update x to u, only element (i + 1) is updated  */
      shared_arr[i + 1] += (sign * l1_norm);

      /* get sum of squares for u, i.e., norm(u) ^ 2  */
      scaler1 += (shared_arr[i + 1] * shared_arr[i + 1]);
      
    }
    __syncthreads();

    /* create vector p <- A %*% u / (norm(u)^2 / 2) */
    data_index = thread_index;
    while (data_index < matrix_arg.rdim) {
      
      /* vector p is 0 for all indices < i  */
      if (data_index < i) {
        linalg_vec[data_index] = 0;
      }

      /* Calculate p[data_index] <- A[data_index,] %*% u / (norm(u)^2 / 2)  */
      else {

        /* initialize matrix multiplication result  */
        linalg_vec[data_index] = 0;

        /* vector u is 0 for index [0 - i], only need to multiply and sum (i + 1) onwards  */
        for (int j = (i + 1); j < matrix_arg.rdim; j++) {

          /* data_index is equiavalent to row_index for this loop */
          linalg_vec[data_index] += (gpu_tridiagonal[(j * matrix_arg.rdim) + data_index] * 
                                     shared_arr[j]);
        }

        /* Divide p[data_index] by H, with H <- norm(u)^2 / 2  */
        linalg_vec[data_index] /= (scaler1 / 2);
      }

      data_index += THREADS_PER_BLOCK;
    }
    __syncthreads();

    /* Calculate constant K <- u %*% p / (norm(u)^2)                          */
    /* Again, not a parallel operation, only first thread of each block used  */
    if (thread_index == DEFAULT_DATA_INDEX) {
      
      /* Store H = norm(u)^2 / 2 before calculating u %*% p */
      scaler1 /= 2;
      scaler2 = 0;
      
      /* vector u is 0 for index [0 - i], only need to multiply and sum (i + 1) onwards  */
      for (int j = (i + 1); j < matrix_arg.rdim; j++) {
        scaler2 += (shared_arr[j] * linalg_vec[j]);
      }

      /* Use shared var to store K by dividing by 2H  */
      scaler2 /= (scaler1 * 2);
    }
    __syncthreads();

    /* Calculate vector q <- p - K * u  */
    data_index = thread_index;
    while (data_index < matrix_arg.rdim) {
      
      /* ovewrite vector p in linalg_vec with vector q  */
      linalg_vec[data_index] -= (scaler2 * shared_arr[data_index]);
    }
    __syncthreads();

    /* Update tridiagonal matrix with computationally cheap but equivalent formula  */
    /* Naive formula:  A' <- P %*% A %*% P with P <- diag(n) - (u %*% t(u) / H)     */
    /* Computationally useful formula: A' <- A - (q %*% t(u)) - (u %*% t(q))        */
    /* The second formula can be calculated in place with only 2 multiplications    */
    /* Vector q is stored in linalg_vec, while vector u is stored in shared_arr     */
    data_index = grid_index;
    row_index = data_index % matrix_arg.rdim;
    col_index = data_index / matrix_arg.cdim;
    for (int j = 0; j < evals_per_thread; j++) {
      
      /* Check overflow */
      if (data_index > matrix_arg.len) break;

      /* Update tridiagonal matrix A' */
      gpu_tridiagonal[data_index] -= (linalg_vec[row_index] * shared_arr[col_index] +
                                      shared_arr[row_index] * linalg_vec[col_index]);

      data_index += grid_size;
      row_index = data_index % matrix_arg.rdim;
      col_index = data_index / matrix_arg.cdim;
    }

    /* Update accumulating matrix Q with Q' <- Q %*% P, do not explicitly create P  */
    /* in any memory as we can avoid global memory reads by instead performing      */
    /* repeated multiplications for each index of P that is recalculated, store the */
    /* final result in global evaluation memory before writing it back to gpu_Q     */
    data_index += grid_size;
    row_index = data_index % matrix_arg.rdim;
    col_index = data_index / matrix_arg.cdim;
    for (int j = 0; j < evals_per_thread; j++) {

      /* Check overflow */
      if (data_index > matrix_arg.len) break;

      /* Update global memory evaluations */
      eval_memory[data_index] = 0;
      for (int k = 0; k < matrix_arg.rdim; k++) {

        /* determine base_p for base_p - u[id1] * u[id2]  */
        // double base_p = (row_index == col_index) ? 1.0 : 0.0;
        double p = (k == col_index) ? 1.0 : 0.0;
        p -= (shared_arr[k] * shared_arr[col_index] / scaler1);
        eval_memory[data_index] += (gpu_Q[(k * matrix_arg.rdim) + row_index] * p);
      }

      data_index += grid_size;
      row_index = data_index % matrix_arg.rdim;
      col_index = data_index / matrix_arg.cdim;
    }
    grid.sync();

    /* Write the evaluated data back to gpu_Q */
    data_index = grid_index;
    for (int j = 0; j < evals_per_thread; j++) {
      gpu_Q[data_index] = eval_memory[data_index];
      data_index += grid_size;
    }
  }
}


/*
 * Top level function to sample from multivariate normal distribution 
 */

__device__ void mvrnorm_device(double* result, double* means, Rvar covar_matrix, 
                               double* gpu_Q, double* gpu_tridiagonal, double* gpu_eigvecs, 
                               double* gpu_eigvals, double* shared_arr, 
                               double* scratch_memory, double* linalg_vec, 
                               int grid_size, int grid_index, int thread_index, 
                               int evals_per_thread, cooperative_groups::grid_group grid) {
  
  /* First find eigenvectors and eigenvalues of the covariance matrix */
  
  /* Step 1 is to reduce matrix to tridiagonal form and save accumlated matrix Q  */
  householder_reduction(covar_matrix, gpu_Q, gpu_tridiagonal, shared_arr, scratch_memory, 
                        linalg_vec, grid_size, grid_index, thread_index, evals_per_thread, 
                        grid);

  
  

}


/*
 * Kernel function ran on the GPU
 */

__global__
void kernel(int grid_size, unsigned long long random_seed, double* scratch_gpu_memory,
            double* gpu_Q, double* gpu_tridiagonal, double* gpu_eigvecs, double* gpu_eigvals)
{
  /* Shared memory used for storage of evaluations or temporarily saved data, */
  /* such as the column of interest in parallel Gauss-Jordan inverse          */
  __shared__ double shared_arr[THREADS_PER_BLOCK * MAX_EVALS_PER_THREAD];
  double* temp_evals = shared_arr;

  /* Linear algebra __shared__ storage, due to hardware this is limited */
  /* Only necessary when more than one vector must be stored for linear */
  /* algebra functions such as reduction to tridiagonal form            */
  __shared__ double linalg_vec[MAX_LINALG_DIM];

  /* The indices that identify both thread index (repeated over blocks) */
  /* and the unique grid index that each thread posseses                */
  const int grid_index = blockDim.x * blockIdx.x + threadIdx.x;
  const int thread_index = threadIdx.x;

  /* Local indices used to temporarily store evaluated values before  */
  /* writing them back to the global memory of the associated Rvar    */
  int _storage_index = thread_index;
  int _storage_inc = THREADS_PER_BLOCK;
  int _eval_data_index = grid_size;
  int _guard_len = 0;

  /* Initialized the group on all threads to allow grid level synchronization */
  cooperative_groups::grid_group grid = cooperative_groups::this_grid();
  
  /* Initialize random state for RNG  */
  curandStateXORWOW_t* grid_state = (curandStateXORWOW_t*) 
                                     malloc(sizeof(curandStateXORWOW_t));
  curand_init(random_seed, grid_index, 0, grid_state);


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

  // CHECK HOW EFFICIENT 2 BLOCKS_PER_SM IS WITH SIMILAR SHARED MEMSIZE PER BLOCK
  printf("Launching %d blocks with %d threads per block\n",
         deviceProp.multiProcessorCount * BLOCKS_PER_SM, THREADS_PER_BLOCK);
  printf("Maximum concurrent evaluation of %d evals per thread\n", 
         max_evals);
  

  /* Retrieve random seed from R  */
  unsigned long long random_seed = 420;

  int linalg_dim = 1;  /* R::linalg_dim */ 

  /* Allocate background memory on GPU  */
  allocate_background_mem(max_evals * deviceProp.multiProcessorCount * BLOCKS_PER_SM * 
                          THREADS_PER_BLOCK, linalg_dim);

  /* Create argument array to be passed to kernel */
  void* args[] = {&grid_size, &random_seed, &scratch_gpu_memory, gpu_Q, gpu_tridiagonal,
                  gpu_eigvecs, gpu_eigvals};

  /* Launch kernel with cooperative groups, allowing grid wide synchronization  */
  cudaLaunchCooperativeKernel((void*) kernel, deviceProp.multiProcessorCount * BLOCKS_PER_SM, 
                              THREADS_PER_BLOCK, args);

  /* Require GPU synchronize before CPU resume execution  */
  cudaDeviceSynchronize();

  /* Check for any errors during kernel launch  */
  cudaError_t err = cudaGetLastError();
  if (err != cudaSuccess) {
    printf("CUDA error: %s\n", cudaGetErrorString(err));
    return;
  }

  /* Clean up memory from intermediate evaluations on GPU */
  free_background_mem();
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
  g_evals_per_thread[/*x*/] = ceil((float) expr_len / grid_size);
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
 * Allocates global memory used for background storage
 */

void allocate_background_mem(int max_eval_size, int linalg_dim){
  scratch_gpu_memory = (double*) malloc_device(max_eval_size * sizeof(double));
  gpu_Q = (double*) malloc_device(pow(linalg_dim, 2) * sizeof(double));
  gpu_tridiagonal = (double*) malloc_device(pow(linalg_dim, 2) * sizeof(double));
  gpu_eigvecs = (double*) malloc_device(pow(linalg_dim, 2) * sizeof(double));
  gpu_Q = (double*) malloc_device(linalg_dim * sizeof(double));
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
 * Frees all non variable memory allocated on GPU
 */

void free_background_mem() {
  free_int_evals();
  free_device(gpu_Q);
  free_device(gpu_tridiagonal);
  free_device(gpu_eigvecs);
  free_device(gpu_eigvals);
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


