#define BLOCKS_PER_SM (1)
#define THREADS_PER_BLOCK (256)
#define MAX_EVALS_PER_THREAD (22)
#define MAX_LINALG_DIM (500)     /* Defines max matrix dim for linalg functions  */

/* Global background memory storage */
extern gpu_store g_mem;

