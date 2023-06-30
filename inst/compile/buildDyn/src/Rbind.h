/* Contains constants and struct definitions for binding R objects to compiled code */

#define ROW_DIM (0)
#define COL_DIM (1)
#define DEFAULT_DATA_INDEX (0)
#define MAX_VARS (100)
#define MAX_ITERS (10)

/* Structure type which holds all presevered information about a corresponding R object */
typedef struct Rvar_info{
  double* data;
  int len;
  int rdim;
  int cdim;
} Rvar;


/* Global array of pointers to R objects bound in compiled memory */
/* and the number of the R object pointers                        */
extern Rvar g_vars[MAX_VARS];
extern int g_var_count;

/* Global array of sizes for each looping iteration data vector */
extern int g_iter_lens[MAX_ITERS];
extern int g_iter_count;


