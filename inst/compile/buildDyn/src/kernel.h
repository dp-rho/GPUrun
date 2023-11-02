#define BLOCKS_PER_SM (1)
#define THREADS_PER_BLOCK (256)
#define MAX_EVALS_PER_THREAD (22)
#define MAX_LINALG_DIM (1000)     /* Defines max matrix dim for linalg functions  */

/* Memory to be allocated on the GPU that is only used in scratch calculations  */
extern double* scratch_gpu_memory;

/* Memory for linear algebra functions  */
extern double* gpu_Q;
extern double* gpu_tridiagonal;
extern double* gpu_eigvecs;
extern double* gpu_eigvals;
