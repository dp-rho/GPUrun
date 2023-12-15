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
/* before a global variable is updated with the evaluated values, as well as linear   */
/* algebgra functions.                                                                */
gpu_store g_mem;
__constant__ gpu_store gpu_mem;


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
  static double q[] =
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
  double ainv = 1.0 / a;
  double x, rho;
  do {
    x = rexp_device(ainv, random_state) + a;
    rho = exp(-0.5 * pow((x - a), 2));
  } while (runif_device(0, 1, random_state) > rho);
  return x;
}

/* Exponential rejection sampling (a,b) */
__device__ double ers_a_b(double a, double b, curandState_t* random_state) {
  double ainv = 1.0 / a;
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
  
  double phi_a = dnorm_device(a, 0, 1);
  double x = 0.0, u = 0.0;

  /* Upper bound of normal density on [a, b] */
  double ub = a < 0 && b > 0 ? M_1_SQRT_2PI : phi_a;
  do {
    x = runif_device(a, b, random_state);
  } while (runif_device(0, 1, random_state) * ub > dnorm_device(x, 0, 1));
  return x;
}

/* Truncated on the left  */
__device__ double rtruncnorm_left(double a, double mean, double sd, 
                                  curandState_t* random_state) {
  double alpha = (a - mean) / sd;
  if (alpha < T4) {
    return mean + sd * nrs_a_inf(alpha, random_state);
  } else {
    return mean + sd * ers_a_inf(alpha, random_state);
  }
}

/* Truncated on the right */
__device__ double rtruncnorm_right(double b, double mean, double sd, 
                                   curandState_t* random_state) {
  double beta = (b - mean) / sd;
  return mean - sd * rtruncnorm_left(-beta, 0.0, 1.0, random_state);
}

/* General case */
__device__ double rtruncnorm_general(double a, double b, double mean, double sd,
                                     curandState_t* random_state) {
  double alpha = (a - mean) / sd;
  double beta = (b - mean) / sd;
  double phi_a = dnorm_device(alpha, 0.0, 1.0);
  double phi_b = dnorm_device(beta, 0.0, 1.0);
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

  int a_finite = (a == -DBL_MAX) ? 0 : 1;
  int b_finite = (b == DBL_MAX) ? 0 : 1;

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

__device__ void inverse(Rvar matrix_arg,
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
    gpu_mem.gpu_scratch_memory[data_index] = matrix_arg.data[data_index];
    
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
    double divisor = gpu_mem.gpu_scratch_memory[(zero_col * matrix_arg.rdim) + zero_col];
    if (grid_index < matrix_arg.cdim) {
      gpu_mem.gpu_scratch_memory[(grid_index * matrix_arg.rdim) + zero_col] /= divisor;
    }
    else if (grid_index < 2 * matrix_arg.cdim) {
      working_result[(grid_index - matrix_arg.cdim) * matrix_arg.rdim + zero_col] /= divisor;
    }
    grid.sync();

    /* Save the zeroing column in shared mem in each block  */
    /* NOTE: Not supported for matrices with more than 256 * MAX_EVALS_PER_THREAD rows  */
    data_index = thread_index;
    while (data_index < matrix_arg.rdim) {
      shared_mem_arr[data_index] = gpu_mem.gpu_scratch_memory[(zero_col * matrix_arg.rdim) + 
                                                              data_index];
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
        gpu_mem.gpu_scratch_memory[data_index] -= (gpu_mem.gpu_scratch_memory[col_index * 
                                                     matrix_arg.rdim + zero_col] *
                                                   shared_mem_arr[row_index]);
        working_result[data_index] -= (working_result[col_index * matrix_arg.rdim + 
                                                      zero_col] * shared_mem_arr[row_index]);
      }

      data_index += grid_size;
    }
    grid.sync();
  }
  
}


/*
 * Solve the symmetric eigenproblem using reduction to tridiagonal form
 * followed by the divide and conquer algorithm
 */


/*
 * Reduces symmetric PSD matrix argument to tridiagonal form and also stores Q,
 * with Q = P(n-2) %*% P(n-3) ... P(1) where P is the householder matrix used
 * to reduce input matrix at each iteration.
 * NOTE: No output is produced by this function, the global variables of 
 * gpu_Q and gpu_tridiagonal are updated.
 */

__device__ void householder_reduction(Rvar matrix_arg, double* shared_arr,  
                                      double* linalg_vec, int grid_size,
                                      int grid_index, int thread_index, int evals_per_thread,
                                      cooperative_groups::grid_group grid) {
  
  int row_index = grid_index % matrix_arg.rdim;
  int col_index = grid_index / matrix_arg.cdim;
  int data_index = grid_index;
  int num_blocks = grid_size / THREADS_PER_BLOCK;
  __shared__ double scaler1;
  __shared__ double scaler2;

  /* First initialize global memory for Q matrix and tridiagonal matrix */
  for (int i = 0; i < evals_per_thread; i++) {
    
    /* Check potential overflow */
    if (data_index >= matrix_arg.len) break;

    /* Initialize matrix that will store tridiagonal results  */
    gpu_mem.gpu_tridiagonal[data_index] = matrix_arg.data[data_index];

    /* Initialize Q to identity matrix */
    if (row_index == col_index) {
      gpu_mem.gpu_Q[data_index] = 1;
    }
    else {
      gpu_mem.gpu_Q[data_index] = 0;
    }

    data_index += grid_size;
    row_index = data_index % matrix_arg.rdim;
    col_index = data_index / matrix_arg.cdim;
  }

  /* Sync all threads after initialzation */
  grid.sync();

  /* Loop n-2 times to transform to tridiagonal form using householder matrices */
  for (int i = 0; i < (matrix_arg.rdim - 2); i++ ) {

    /* Size of current iteration householder matrix */
    int size_Q = (matrix_arg.rdim - i - 1);

    /* vectors x, u, p and q are all created in shared memory across all SMs  */
     
    /* create vector x as first n - (i + 1) elements of current matrix  */
    data_index = thread_index;
    while (data_index < size_Q) {
      linalg_vec[data_index] = gpu_mem.gpu_tridiagonal[(size_Q * matrix_arg.rdim) + 
                                                       data_index];
      data_index += THREADS_PER_BLOCK;
    }
    __syncthreads();

    /* create vector u <- x + (sign) * norm(x) * e1, u overwrites x in shared_arr */
    /* This is not a parallel operation, as norm is sequential and only one index */
    /* is updated in the vector x, thus only the first thread of each block used  */
    if (thread_index == DEFAULT_DATA_INDEX) {
 
      /* determine sign for u = x + (sign) * e1 * norm(x) */
      int sign = (gpu_mem.gpu_tridiagonal[0] > 0) ? 1 : -1;
 
      scaler1 = 0;
      for (int j = 0; j < size_Q; j++) {
        scaler1 += (linalg_vec[j] * linalg_vec[j]);
      }

      /* Get norm of x  */
      double l1_norm = sqrt(scaler1);

      /* subtract the squared element that will be updated to create u  */
      scaler1 -= (linalg_vec[size_Q - 1] * linalg_vec[size_Q - 1]);

      /* update x to u, only element (i + 1) is updated  */
      linalg_vec[size_Q - 1] += (sign * l1_norm);

      /* get sum of squares for u, i.e., norm(u) ^ 2  */
      scaler1 += (linalg_vec[size_Q - 1] * linalg_vec[size_Q - 1]);

      /* set u index out of bounds to 0 */
      linalg_vec[size_Q] = 0;
      
    }
    __syncthreads();

    /* create vector p <- A %*% u / (norm(u)^2 / 2) */
    data_index = thread_index;
    while (data_index < (size_Q + 1)) {
      
      /* Calculate p[data_index] <- A[data_index,] %*% u / (norm(u)^2 / 2)  */

      /* initialize matrix multiplication result  */
      shared_arr[data_index] = 0;

      /* vector u is 0 for index >= size_Q  */
      for (int j = 0; j < size_Q; j++) {

        /* data_index is equiavalent to row_index for this loop */
        shared_arr[data_index] += (gpu_mem.gpu_tridiagonal[(j * matrix_arg.rdim) + 
                                                           data_index] *
                                   linalg_vec[j]);
      }

      /* Divide p[data_index] by H, with H <- norm(u)^2 / 2  */
      shared_arr[data_index] /= (scaler1 / 2);

      data_index += THREADS_PER_BLOCK;
    }
    __syncthreads();

    /* Calculate constant K <- u %*% p / (norm(u)^2)                          */
    /* Again, not a parallel operation, only first thread of each block used  */
    if (thread_index == DEFAULT_DATA_INDEX) {
      
      /* Store H = norm(u)^2 / 2 before calculating u %*% p */
      scaler1 /= 2;
      scaler2 = 0;
      
      /* vector u is 0 for index > size_Q */
      for (int j = 0; j < size_Q; j++) {

        /* Sum product of u[j] * p[j] */
        scaler2 += (linalg_vec[j] * shared_arr[j]);
      }

      /* Use shared var to store K by dividing by 2H  */
      scaler2 /= (scaler1 * 2);
    }
    __syncthreads();

    /* Calculate vector q <- p - K * u  */
    data_index = thread_index;
    while (data_index < (size_Q + 1)) {
      
      /* ovewrite vector p in shared_arr with vector q  */
      shared_arr[data_index] -= (scaler2 * linalg_vec[data_index]);
      data_index += THREADS_PER_BLOCK;
    }
    __syncthreads();

    
    /* Update tridiagonal matrix with computationally cheap but equivalent formula  */
    /* Naive formula:  A' <- P %*% A %*% P with P <- diag(n) - (u %*% t(u) / H)     */
    /* Computationally useful formula: A' <- A - (q %*% t(u)) - (u %*% t(q))        */
    /* The second formula can be calculated in place with only 2 multiplications    */
    /* Vector q is stored in linalg_vec, while vector u is stored in shared_arr     */
    data_index = grid_index;
    row_index = data_index % matrix_arg.rdim;
    col_index = data_index / matrix_arg.rdim;
    for (int j = 0; j < evals_per_thread; j++) {
     
      /* Check overflow */
      if (data_index >= matrix_arg.len) break;

      double qtu = (row_index <= size_Q && col_index < size_Q) ? 
              shared_arr[row_index] * linalg_vec[col_index] : 0;
      double utq = (row_index < size_Q && col_index <= size_Q) ?
              linalg_vec[row_index] * shared_arr[col_index] : 0;


      /* Update tridiagonal matrix A' <- A - (q %*% t(u)) - (u %*% t(q) */
      gpu_mem.gpu_tridiagonal[data_index] -= (qtu + utq);
   
      data_index += grid_size;
      row_index = data_index % matrix_arg.rdim;
      col_index = data_index / matrix_arg.rdim;
    }
    grid.sync();

    /* Update accumulating matrix Q with Q' <- Q %*% P, do not explicitly create P  */
    /* in any memory as we can avoid global memory reads by instead performing      */
    /* repeated multiplications for each index of P that is recalculated            */

    /* Computationally useful formula is Q` <- Q - ((Q %*% u) %*% t(u) / H) */

    /* First compute Q %*% u and store in shared_arr  */
    data_index = thread_index;
    while (data_index < matrix_arg.rdim) {
      
      /* Accumulation of cross product  */
      double res = 0.0;
      
      /* vector u is 0 for indices >= size_Q  */
      for (int j = 0; j < size_Q; j++) {
        res += gpu_mem.gpu_Q[(j * matrix_arg.rdim) + data_index] * linalg_vec[j];
      }

      /* store index of Q %*% u in shared_arr */
      shared_arr[data_index] = res;

      data_index += THREADS_PER_BLOCK;
    }
    __syncthreads();

    /* Update using Q` <- Q - ((Q %*% u) %*% t(u) / H)  */
    data_index = grid_index;
    row_index = data_index % matrix_arg.rdim;
    col_index = data_index / matrix_arg.rdim;
    for (int j = 0; j < evals_per_thread; j++) {
     
      /* Check overflow */
      if (data_index >= matrix_arg.len) break;
 
      /* Vector u is 0 for indices >= size_Q  */
      double tu = (col_index < size_Q) ? linalg_vec[col_index] : 0.0;
      gpu_mem.gpu_Q[data_index] -= (shared_arr[row_index] * tu / scaler1);
      
      data_index += grid_size;
      row_index = data_index % matrix_arg.rdim;
      col_index = data_index / matrix_arg.rdim;
    }
    grid.sync(); 
  }

}


/* Secular function used to find eigenvalues of Diagonal + rank1 update matrix */
__device__ double secular_fx(double x, double* w, int start, int stop,
                             double* cur_eigens, int sign_rho) {
  double sum = sign_rho;
  for (int i = start; i < stop; i++) {
    sum += (w[i] * w[i]) / (cur_eigens[i] - x);
  }
  return sum;
}

/* sign function */
__device__ double sign(double n) {
  return n > 0 ? 1 : -1;
}


/*
 * Solves the eigenproblem of a diagonal matrix with a rank one update using bisection to
 * find the roots of the secular equation for lambda_i between diagonal elements.  The 
 * calculated eigenvalues will be stored in gpu_mem.gpu_eigenvalues, while the calculated
 * eigenvectors of the rank1 updated matrix are stored in gpu_mem.gpu_Qprime.
 */

__device__ void solve_rank1_update(int grid_index, int thread_index, int grid_size, 
                                   int mat_dim, double* off_diag_eles, double* v, 
                                   int merge_size,
                                   cooperative_groups::grid_group grid) {

  int data_index = grid_index;
  int row_index = data_index % mat_dim;
  int col_index = data_index / mat_dim;
  int data_size = mat_dim * mat_dim;
  double* cur_eigens = gpu_mem.gpu_eigvalues;
  
  /* init matrix Qprime to identity matrix  */
  while (data_index < data_size) {
    if (row_index == col_index) {
      gpu_mem.gpu_Qprime[data_index] = 1;
    }
    else {
      gpu_mem.gpu_Qprime[data_index] = 0;
    }
    data_index += grid_size;
    row_index = data_index % mat_dim;
    col_index = data_index / mat_dim;
  }
  grid.sync();

  /* Update v to w based on rho */
  data_index = thread_index;
  while (data_index < mat_dim) {
    int block_sub_problem_offset = floor((float) (data_index / (merge_size * 2))) * 
                                    (merge_size * 2);
    int block_merge_index = block_sub_problem_offset + merge_size - 1;
    int block_sign_rho = sign(off_diag_eles[block_merge_index]);

    /* We now transform v to w such that rho = 1 for the matrix D - rho * v %*% t(v)  */
    v[data_index] = block_sign_rho * sqrt(abs(off_diag_eles[block_merge_index])) * 
                     v[data_index];

    data_index += THREADS_PER_BLOCK;
  }
 
  /* Begin process to calculate lambda_i for each thread  */
  int sub_problem_offset = floor((float) (grid_index / (merge_size * 2))) * (merge_size * 2);
  int merge_index = min(sub_problem_offset + merge_size - 1, mat_dim - 1);
  int sub_problem_end = min(sub_problem_offset + (2 * merge_size), mat_dim);
  int sign_rho = sign(off_diag_eles[merge_index]);

  /* Exclude cases where no calculation is needed for this thread until after bisection */
  if (grid_index < mat_dim && merge_index < (mat_dim - 1) && 
      off_diag_eles[merge_index] != 0) {

    /* The calculated eigenvalue lambda_i for the appropriate level sub problem */
    double lambda_i = 0;
    
    /* Precision used to calculate eigenvalues  */
    double machine_prec = 1e-15;

    /* Variables used in bisection  */
    double x1 = -DBL_MAX;
    double x2 = DBL_MAX;
    double fx1 = 0;
    double fx2 = 0;

    /* Begin execution of finding secular roof for sub problem  */
    if (sign_rho < 0) {
      
      /* Find the previous pole lambda_kprev such that lambda_i is between  */
      /* the current diagonal value and lambda_kprev                        */
      double lambda_kprev = -DBL_MAX;
      double norm_w = 0;
      for (int j = sub_problem_offset; j < sub_problem_end; j++) {
        norm_w += cur_eigens[j] * cur_eigens[j];
        if (cur_eigens[j] < cur_eigens[grid_index] && cur_eigens[j] > lambda_kprev)
          lambda_kprev = cur_eigens[j];
      }
      
      /* Suggested offset for when looking for root smaller than the smallest diagonal  */
      if (lambda_kprev == -DBL_MAX) {
        norm_w = sqrt(norm_w);
        lambda_kprev = cur_eigens[grid_index] - norm_w;
      }

      /* Find the closest values (by specified precision) between the two poles of interest */
      double closest_cur = cur_eigens[grid_index] - machine_prec;
      double closest_prev = lambda_kprev + machine_prec;
      double scaler = 1.0;

      /* Precision is finite, may need to increase offset if current diagonal is large  */
      while (closest_cur == cur_eigens[grid_index]) {
        scaler *= 10.0;
        closest_cur = cur_eigens[grid_index] - (machine_prec * scaler);
      }
      scaler = 1.0;
      while (closest_prev == lambda_kprev) {
        scaler *= 10;
        closest_prev = cur_eigens[grid_index] + (machine_prec * scaler);
      }

      /* If the eigenvalue lies between the pole and closest machine value, take the pole */
      /* to be the eigenvalue and the eigenvector is the unit vector at index i           */
      fx2 = secular_fx(closest_cur, v, sub_problem_offset, sub_problem_end,
                       cur_eigens, sign_rho);
      fx1 = secular_fx(closest_prev, v, sub_problem_offset, sub_problem_end,
                       cur_eigens, sign_rho);
      if (fx2 < 0) {
        lambda_i = cur_eigens[grid_index];
      }
      else if (fx1 > 0) {
        lambda_i = lambda_kprev;
      }
      else {
        x1 = closest_prev;
        x2 = closest_cur;
      }
    }

    /* Case where rho is positive */
    else {

      /* Repeat above code but with directionality switched, should probably be cleaned up  */
      double lambda_knext = DBL_MAX;
      double norm_w = 0;
      for (int j = sub_problem_offset; j < sub_problem_end; j++) {
        norm_w += cur_eigens[j] * cur_eigens[j];
        if (cur_eigens[j] > cur_eigens[grid_index] && cur_eigens[j] < lambda_knext)
          lambda_knext = cur_eigens[j];
      }
      if (lambda_knext == DBL_MAX) {
        norm_w = sqrt(norm_w);
        lambda_knext = cur_eigens[grid_index] + norm_w;      
      }
      double closest_cur = cur_eigens[grid_index] + machine_prec;
      double closest_next = lambda_knext - machine_prec;
      double scaler = 1.0;

      while (closest_cur == cur_eigens[grid_index]) {
        scaler *= 10.0;
        closest_cur = cur_eigens[grid_index] + (machine_prec * scaler);
      }
      scaler = 1.0;
      while (closest_next == lambda_knext) {
        scaler *= 10.0;
        closest_next = lambda_knext - (machine_prec * scaler);
      }

      fx1 = secular_fx(closest_cur, v, sub_problem_offset, sub_problem_end,
                       cur_eigens, sign_rho);
      fx2 = secular_fx(closest_next, v, sub_problem_offset, sub_problem_end,
                       cur_eigens, sign_rho);
      if (fx1 > 0) {
        lambda_i = cur_eigens[grid_index];
      }
      else if (fx2 < 0) {
        lambda_i = lambda_knext;
      }
      else {
        x1 = closest_cur;
        x2 = closest_next;
      }
    }

    /* In case where root was taken to be one of the poles, no calculation needed  */
    if (x1 != -DBL_MAX) {

      /* Begin bisection code */
      double xm = (x1 + x2) / 2;
      int bisect_iter = 0;
      double fxm = secular_fx(xm, v, sub_problem_offset, sub_problem_end,       
                                cur_eigens, sign_rho);
      while (x1 < xm && xm < x2 && bisect_iter < 500)  {
        if (fxm == 0) 
          break;

        if (sign(fx1) != sign(fxm)) {
          x2 = xm;
        }
        else {
          x1 = xm;
          fx1 = fxm;
        }
        xm = (x1 + x2) / 2;
        fxm = secular_fx(xm, v, sub_problem_offset, sub_problem_end,
                                cur_eigens, sign_rho);
        bisect_iter++;
      }
      lambda_i = xm;
    }

    /* Store calculated eigenvalue global scratch memory  */
    gpu_mem.gpu_scratch_memory[grid_index] = lambda_i;
  }

  /* No calculation needed case, simply use old eigenvalue  */
  else if (grid_index < mat_dim) {
    gpu_mem.gpu_scratch_memory[grid_index] = gpu_mem.gpu_eigvalues[grid_index];
  }
  grid.sync();
  
  /* Calculate gpu_Qprime for each merging subproblem */
 
  /* The number of indices in Qprime that will be updated is based on merge_size  */
  int full_problems = floor((float) (mat_dim / (2 * merge_size)));
  int remaining_dim = mat_dim - (full_problems * (2 * merge_size));
  int merge_data_size = (4 * merge_size * merge_size);
  data_size = full_problems * merge_data_size + (remaining_dim * remaining_dim);
  data_index = grid_index;
  while (data_index < data_size) {

    /* Index sub problem this thread is contributing to, each merged sub problem has  */
    /* dimension of (2 * merge_size), thus there are (2 * merge_size)^2 elements      */
    int sub_problem_index = floor((float) (data_index / merge_data_size));
    
    /* Init sub problem row and column indices  */
    int sub_row_index = (data_index - (sub_problem_index * merge_data_size));
    int sub_col_index = (data_index - (sub_problem_index * merge_data_size));

    /* If we are at the last sub problem, it may not be "default" size  */
    if (sub_problem_index != full_problems) {
      sub_row_index %= (2 * merge_size);
      sub_col_index /= (2 * merge_size);
    }
    else {
      sub_row_index %= remaining_dim;
      sub_col_index /= remaining_dim;
    }
    
    /* Offset in 1 dimension  */
    sub_problem_offset = sub_problem_index * (2 * merge_size);
    int sub_problem_end = min(sub_problem_offset + (2 * merge_size), mat_dim);

    int row_index = sub_row_index + sub_problem_offset;
    int col_index = sub_col_index + sub_problem_offset;
    
    /* Eigenvalue that was calculated is stored at col_index of scratch memory  */
    /* If calculated eigenvalue exists in previous eigenvalues (poles) do not   */
    /* calculate and instead use the initialized unit vector in Qprime          */
    int root_exists = 0;
    for (int i = sub_problem_offset; i < sub_problem_end; i++) {
      if (gpu_mem.gpu_scratch_memory[col_index] == cur_eigens[i])
        root_exists = 1;
    }
    if (!root_exists) { 
      double vhat = (1 / (gpu_mem.gpu_scratch_memory[col_index] - 
                          cur_eigens[row_index]) * 
                     v[row_index]);
      gpu_mem.gpu_Qprime[(col_index * mat_dim) + row_index] = vhat;
    }

    data_index += grid_size;
  }
  grid.sync();

  /* Divide each vector vhat by norm(vhat) after initial calculation  */
  if (grid_index < mat_dim) {
    double vhat_norm = 0;
    int sub_problem_index = floor((float) (grid_index / (2 * merge_size)));
    sub_problem_offset = sub_problem_index * (2 * merge_size);
    int upper_bound = min(sub_problem_offset + (2 * merge_size), mat_dim);
    for (int i = sub_problem_offset; i < upper_bound; i++) {
      vhat_norm += pow((gpu_mem.gpu_Qprime[(grid_index * mat_dim) + i]), 2);
    }
    vhat_norm = sqrt(vhat_norm);
    for (int i = sub_problem_offset; i < upper_bound; i++) {
      gpu_mem.gpu_Qprime[(grid_index * mat_dim) + i] /= vhat_norm;
    }
  }

  /* Update eigenvalues */
  if (grid_index < mat_dim) {
    gpu_mem.gpu_eigvalues[grid_index] = gpu_mem.gpu_scratch_memory[grid_index];
  }
  grid.sync();
  
}


/*
 * Uses divide and conquer to solve symmetric tridiagonal eigenproblem, the eigenvalues
 * are stored in gpu_mem.gpu_eigenvalues, the eigenvectors of the tridiagonal are stored
 * in gpu_mem.gpu_eigenvectors, which can then be transformed with accumulated matrix Q
 * from the cumulative householder reductions to retrieve original matrix eigenvectors
 */

__device__ void tri_eigen_divide_conquer(int grid_index, int thread_index, int grid_size,
                                         int mat_dim, double* linalg_vec, double* shared_arr,
                                         cooperative_groups::grid_group grid) {

  
  /* Init the diag elements of the matrix to gpu_mem.gpu_eigenvalues */
  if (grid_index < mat_dim){ 
    gpu_mem.gpu_eigvalues[grid_index] = gpu_mem.gpu_tridiagonal[(grid_index * mat_dim) + 
                                                                 grid_index];
  }

  /* Init the off diag elements in linalg_vec which is __shared__ memory */
  int data_index = thread_index;
  while (data_index < mat_dim) {
    linalg_vec[data_index] = gpu_mem.gpu_tridiagonal[((data_index + 1) * mat_dim) + 
                                                     data_index];
    data_index += THREADS_PER_BLOCK;
  }
  grid.sync();

  /* Init the matrix that will hold sub problem eigenvectors as they are calculated */
  data_index = grid_index;
  int data_size = mat_dim * mat_dim;
  while (data_index < data_size) {
    int row_index = data_index % mat_dim;
    int col_index = data_index / mat_dim;
    if (row_index == col_index) {
      gpu_mem.gpu_eigvectors[data_index] = 1;
    }
    else {
      gpu_mem.gpu_eigvectors[data_index] = 0;
    }
    data_index += grid_size;
  }

  /* Subtract off diagaonal elements from all sub problem merging indices in two steps    */
  /* to avoid conflicts, as most indices will have off diagonal elements subtracted twice */
  if (grid_index < (mat_dim - 1)) {
    if (grid_index % 2 == 0)
      gpu_mem.gpu_eigvalues[grid_index] -= linalg_vec[grid_index];
    else
      gpu_mem.gpu_eigvalues[grid_index] -= linalg_vec[grid_index - 1];
  }
  grid.sync();

  if (grid_index < (mat_dim - 1)) {
    if (grid_index % 2 == 0 && (grid_index < (mat_dim -2)))
      gpu_mem.gpu_eigvalues[grid_index + 1] -= linalg_vec[grid_index + 1];
    else
      gpu_mem.gpu_eigvalues[grid_index + 1] -= linalg_vec[grid_index];
  }

  /* Begin iterative process of combining sub problems  */
  int merge_size = 1;
  while (merge_size < mat_dim) {
    
    /* First create vector v from last row of Q1 and first row of Q2 for each sub problem */
    data_index = thread_index;
    while (data_index < mat_dim) {
      int sub_problem_id = floor((float) data_index / merge_size);
      int vertical_offset = sub_problem_id * merge_size;

      /* Get last row */
      if (sub_problem_id % 2 == 0) {
        vertical_offset += (merge_size - 1);
      }

      shared_arr[data_index] = gpu_mem.gpu_eigvectors[(data_index * mat_dim) + 
                                                      min(vertical_offset, (mat_dim - 1))];
      data_index += THREADS_PER_BLOCK;
    }
    __syncthreads();

    /* Solve current level of sub problem */
    solve_rank1_update(grid_index, thread_index, grid_size, mat_dim, linalg_vec, 
                       shared_arr, merge_size, grid);

    /* Update gpu_mem.gpu_eigenvectors by multiplying each block diagonal sub problem */
    /* matrix of eigenvectors [Q1, Q2] %*% Qprime                                     */
    int full_problems = floor((float) (mat_dim / (2 * merge_size)));
    int remaining_dim = mat_dim - (full_problems * (2 * merge_size));
    int merge_data_size = (4 * merge_size * merge_size);
    int data_size = full_problems * merge_data_size + (remaining_dim * remaining_dim);
    data_index = grid_index;
    while (data_index < data_size) {

      /* Index sub problem this thread is contributing to, each merged sub problem has  */
      /* dimension of (2 * merge_size), thus there are (2 * merge_size)^2 elements      */
      int sub_problem_index = floor((float) (data_index / merge_data_size));

      /* Init sub problem row and column indices  */
      int sub_row_index = (data_index - (sub_problem_index * merge_data_size));
      int sub_col_index = (data_index - (sub_problem_index * merge_data_size));

      /* If we are at the last sub problem, it may not be "default" size  */
      if (sub_problem_index != full_problems) {
        sub_row_index %= (2 * merge_size);
        sub_col_index /= (2 * merge_size);
      }
      else {
        sub_row_index %= remaining_dim;
        sub_col_index /= remaining_dim;
      }

      /* Offset in 1 dimension  */
      int sub_problem_offset = sub_problem_index * (2 * merge_size);

      int row_index = sub_row_index + sub_problem_offset;
      int col_index = sub_col_index + sub_problem_offset;

      /* Take matrix product of current index using only the merge_data_size sub matrix */
      int upper_bound = min(sub_problem_offset + (2 * merge_size), mat_dim);
      double acc = 0.0;
      for (int i = sub_problem_offset; i < upper_bound; i++) {
        acc += (gpu_mem.gpu_eigvectors[(i * mat_dim) + row_index] * 
                gpu_mem.gpu_Qprime[(col_index * mat_dim) + i]);
      }
   
      /* Store in scratch memory  */
      gpu_mem.gpu_scratch_memory[(col_index * mat_dim) + row_index] = acc;

      data_index += grid_size;
    }
    grid.sync();

    /* Copy scratch memory back to gpu_eigvectors */
    /* Lots of repeated code here, should be cleaned up */
    data_index = grid_index;
    while (data_index < data_size) {

      /* Index sub problem this thread is contributing to, each merged sub problem has  */
      /* dimension of (2 * merge_size), thus there are (2 * merge_size)^2 elements      */
      int sub_problem_index = floor((float) (data_index / merge_data_size));

      /* Init sub problem row and column indices  */
      int sub_row_index = (data_index - (sub_problem_index * merge_data_size));
      int sub_col_index = (data_index - (sub_problem_index * merge_data_size));

      /* If we are at the last sub problem, it may not be "default" size  */
      if (sub_problem_index != full_problems) {
        sub_row_index %= (2 * merge_size);
        sub_col_index /= (2 * merge_size);
      }
      else {
        sub_row_index %= remaining_dim;
        sub_col_index /= remaining_dim;
      }

      /* Offset in 1 dimension  */
      int sub_problem_offset = sub_problem_index * (2 * merge_size);

      int row_index = sub_row_index + sub_problem_offset;
      int col_index = sub_col_index + sub_problem_offset;

      gpu_mem.gpu_eigvectors[(col_index * mat_dim) + row_index] = 
        gpu_mem.gpu_scratch_memory[(col_index * mat_dim) + row_index];

      data_index += grid_size;
    }

    /* Increment the merging size */
    merge_size *= 2;
 
  }

}


/*
 * Top level function to sample from multivariate normal distribution 
 */

__device__ void mvrnorm_device(double* means, Rvar covar_matrix, double* result,
                               double* shared_arr, double* linalg_vec, 
                               int grid_size, int grid_index, 
                               int thread_index, int evals_per_thread, 
                               cooperative_groups::grid_group grid,
                               curandState_t* random_state) {

  /* Find eigenvectors and eigenvalues of the covariance matrix */
  
  /* Step 1 is to reduce matrix to tridiagonal form and save accumlated matrix Q  */
  householder_reduction(covar_matrix, shared_arr, linalg_vec, grid_size,
                        grid_index, thread_index, evals_per_thread, grid);

  /* Step 2 is to solve the symmetric tridiagonal eigenproblem with divide and conquer  */
  tri_eigen_divide_conquer(grid_index, thread_index, grid_size, covar_matrix.rdim, 
                           linalg_vec, shared_arr, grid);
  //return;
  /* Convert eigenvecs of tridiagonal matrix to original eigenvecs with accumulated */
  /* Householder transformations stored in gpu_mem.gpu_Q                            */
  Rvar householder_Q = {
    .data = gpu_mem.gpu_Q,
    .len = covar_matrix.len,
    .rdim = covar_matrix.rdim,
    .cdim = covar_matrix.cdim
  };
  Rvar c_eigvecs = {
    .data = gpu_mem.gpu_eigvectors,
    .len = covar_matrix.len,
    .rdim = covar_matrix.rdim,
    .cdim = covar_matrix.cdim
  };

  /* Perform matrix multiplication on accumulated matrix Q to eigenvectors of the   */
  /* tridiagonal matrix, use global scratch memory as intermediate storage          */
  int data_index = grid_index;
  while (data_index < covar_matrix.len) {
    gpu_mem.gpu_scratch_memory[data_index] = mat_mul(householder_Q, c_eigvecs, data_index);
    data_index += grid_size;
  }
  grid.sync();
  data_index = grid_index;
  while (data_index < covar_matrix.len) {
    gpu_mem.gpu_eigvectors[data_index] = gpu_mem.gpu_scratch_memory[data_index];
    data_index += grid_size;
  }
 
  /* Multiply eigenvectors by sqrt of their eigenvalues */
  data_index = grid_index;
  int col_index = data_index / covar_matrix.rdim;
  while (data_index < covar_matrix.len) {
    gpu_mem.gpu_eigvectors[data_index] *= sqrt(gpu_mem.gpu_eigvalues[col_index]);
    data_index += grid_size;
    col_index = data_index / covar_matrix.rdim;
  }

  /* Get random sample X, currently constrained to 1 sample from mvrnorm  */
  int data_size = covar_matrix.rdim;
  data_index = grid_index;
  while (data_index < covar_matrix.rdim) {
    gpu_mem.gpu_scratch_memory[data_index] = curand_normal_double(random_state);    
    data_index += grid_size;
  }

  /* Mutiply eigvectors %*% t(X)  */
  Rvar tX = {
    .data = gpu_mem.gpu_scratch_memory,
    .len = covar_matrix.rdim,
    .rdim = covar_matrix.rdim,
    .cdim = 1 /* artificial constraint  */
  };

  /* Copy sampled value to result */
  data_index = grid_index;
  while (data_index < covar_matrix.rdim) {
    result[data_index] = mat_mul(c_eigvecs, tX, data_index) + means[data_index];
    data_index += grid_size;
  }

}


/*
 * Kernel function ran on the GPU
 */

__global__
void kernel(int grid_size, unsigned long long random_seed)
{
  /* Shared memory used for storage of evaluations or temporarily saved data, */
  /* such as the column of interest in parallel Gauss-Jordan inverse          */
  __shared__ double shared_arr[THREADS_PER_BLOCK * MAX_EVALS_PER_THREAD];
  double* temp_evals = shared_arr;

  /* Linear algebra __shared__ storage, due to hardware this is limited */
  /* Only necessary when more than one vector must be stored for linear */
  /* algebra functions such as reduction to tridiagonal form            */
  double* linalg_vec = shared_arr + (THREADS_PER_BLOCK * MAX_EVALS_PER_THREAD / 2);

  /* The indices that identify both thread index (repeated over blocks) */
  /* and the unique grid index that each thread posseses                */
  const int grid_index = blockDim.x * blockIdx.x + threadIdx.x;
  const int thread_index = threadIdx.x;
  const int block_index = blockIdx.x;

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

  /* Calculate the number of maximum number of evaluations needed per thread, this    */
  /* is required to allocate background memory and memory to store evaluated elements */
  /* before overwriting the global Rvar memory in assignment. Note that the evals per */ 
  /* thread in each expression will vary as the goal is to maximize concurrency, thus */
  /* larger expressions will require more evaluations per thread.                     */
  cudaDeviceProp deviceProp;
  int dev = 0;
  cudaGetDeviceProperties(&deviceProp, dev);
  int grid_size = deviceProp.multiProcessorCount * BLOCKS_PER_SM * THREADS_PER_BLOCK;

  /* Optimal threads per block and blocks per SM likely varies with hardware  */
  printf("Launching %d blocks with %d threads per block\n",
         deviceProp.multiProcessorCount * BLOCKS_PER_SM, THREADS_PER_BLOCK);
  printf("Maximum concurrent evaluation of %d evals per thread\n", 
         max_evals);
  

  /* Retrieve random seed from R  */
  unsigned long long random_seed = 420;

  // [[Lin.Alg::start]]
  int linalg_dims[] = {/* R::linalg_dims */};
  // [[Lin.Alg::end]]

  int linalg_dim = *(std::max_element(linalg_dims, 
                                      linalg_dims + (sizeof(linalg_dims) / sizeof(int))));

  /* Allocate background memory on GPU  */
  allocate_background_mem(max_evals * deviceProp.multiProcessorCount * BLOCKS_PER_SM * 
                          THREADS_PER_BLOCK, linalg_dim);

  /* Create argument array to be passed to kernel */
  void* args[] = {&grid_size, &random_seed};

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
  
  /* Allocate global memory used for intermediate evaluations, the size required is */
  /* the maximum between the largest evaluations needed for any expression and the  */
  /* matrix size of linear algebra functions called                                 */
  double* g_scratch_memory = (double*) malloc_device(max_eval_size * sizeof(double));

  /* Initialize pointers for global background memory used in linear algebra  */
  double* g_Q = NULL;
  double* g_tridiagonal = NULL;
  double* g_eigvalues = NULL;
  double* g_eigvectors = NULL;
  double* g_Qprime = NULL;
  if (linalg_dim > 0) {
    g_Q = (double*) malloc_device(pow(linalg_dim, 2) * sizeof(double));
    g_tridiagonal = (double*) malloc_device(pow(linalg_dim, 2) * sizeof(double));
    g_eigvalues = (double*) malloc_device(linalg_dim * sizeof(double));
    g_eigvectors = (double*) malloc_device(pow(linalg_dim, 2) * sizeof(double));
    g_Qprime = (double*) malloc_device(pow(linalg_dim, 2) * sizeof(double));
  }

  /* Copy pointers to __constant__ memory on device for fast global access  */
  g_mem = {
    .gpu_scratch_memory = g_scratch_memory,
    .gpu_tridiagonal = g_tridiagonal,
    .gpu_Q = g_Q,
    .gpu_eigvalues = g_eigvalues,
    .gpu_eigvectors = g_eigvectors,
    .gpu_Qprime = g_Qprime,
  };

  cudaError_t err = cudaMemcpyToSymbol(gpu_mem, &g_mem, sizeof(gpu_store));
  if (err != cudaSuccess) {
    printf("CUDA error while copying background gpu pointers to __constant__ memory: %s\n",
           cudaGetErrorString(err));
  }
  cudaDeviceSynchronize();
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
  free_device(g_mem.gpu_scratch_memory);
  if (g_mem.gpu_Q) 
    free_device(g_mem.gpu_Q);
  if (g_mem.gpu_tridiagonal)
    free_device(g_mem.gpu_tridiagonal);
  if (g_mem.gpu_eigvectors)
    free_device(g_mem.gpu_eigvectors);
  if (g_mem.gpu_eigvalues)
    free_device(g_mem.gpu_eigvalues);
  if (g_mem.gpu_Qprime)
    free_device(g_mem.gpu_Qprime);
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


