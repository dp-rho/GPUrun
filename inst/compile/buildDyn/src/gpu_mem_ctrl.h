#define MAX_GPU_POINTERS (500)

/* The array of globally accessible pointers to gpu ready dynamic memory  */
extern void* gpu_mem_ptrs[MAX_GPU_POINTERS];

/* The structure that holds global background memory  */
typedef struct gpu_background_storage{
  double* gpu_scratch_memory;
  double* gpu_tridiagonal;
  double* gpu_Q;
  double* gpu_eigvalues;
  double* gpu_eigvectors;
  double* gpu_Qprime;
} gpu_store;

