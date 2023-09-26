#define BLOCKS_PER_SM (1)
#define THREADS_PER_BLOCK (256)
#define MAX_EVALS_PER_THREAD (22)

/* Memory to be allocated on the GPU that is only used in scratch calculations  */
extern double* scratch_gpu_memory;
