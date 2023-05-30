#include "commands.h"


/* Initialize global array of structures for R objects bound in compiled memory */
/* and the number of the R objects                                              */
Rvar g_vars[MAX_VARS];
int g_var_count = 0;


/* 
 * Binds the abstracted information of an R object into a dynamically 
 * allocated Cpp structure with memory that can be accessed by the GPU.
 * This function is called from R using the Rcpp interface.
 */
// [[Rcpp::export]]
void bind_var(NumericVector var, NumericVector dimensions) {

  /* Initialize the var_info structure which will hold all preserved information */
  Rvar new_var = {
    .data = NULL,
    .len = (int) var.size(),
    .rdim = (int) dimensions[ROW_DIM],
    .cdim = (int) dimensions[COL_DIM]
  };

  /* Allocated the data array for the var_info and copy R object data */
  new_var.data = (double*) malloc_device(sizeof(double) * new_var.len);
  memcpy_to_device(new_var.data, var.begin(), sizeof(double) * new_var.len);

  double result;
  memcpy_to_host(&result, new_var.data, sizeof(double));
  printf("var len: %d\n", new_var.len);
  printf("var data: %f\n", result);

  /* Save the pointer to the var_info in the global variable array  */
  g_vars[g_var_count++] = new_var;
}


/*
 * Extracts the data from a Rvar structure stored
 * in the global variable table at the provided index
 * and returns it to be written to the the associated R object
 */
// [[Rcpp::export]]
NumericVector get_data(int index) {
  Rvar cur_var = g_vars[index - 1];
  NumericVector Rvec(cur_var.len);
  memcpy_to_host(Rvec.begin(), cur_var.data, sizeof(double) * cur_var.len);
  free_device(cur_var.data);
  g_var_count--;
  return Rvec;
}

