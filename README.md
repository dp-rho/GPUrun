# GPUrun
Rcpp package that utilizes CUDA to enable fully out of the box GPU computation of basic R expressions via compilation and linking of machine generated .cu and .cpp code.  Requires CUDA 9.0+ and Linux OS.

## Overview of Implementation and Functionality
GPUrun is designed for high performance execution of R expressions which operate on strictly numerical data of up to two dimensions.  Only functions explicitly listed here can be included in expressions.  It is relatively easy to implement any highly parallel function, although implementing functions which are inherently sequential is possible, it cannot be done efficiently.  In general, expressions compiled and executed by GPUrun should mimic the functionality of native R implementations, with some notable restrictions.

### Restrictions
- Variables and expressions cannot change size during compiled execution.  Dimensions of expressions, and by extension the rules for computation, are parsed only once by the CPU before the CUDA kernel is called. The parsing of dimensions and allocation of data is generally not parallel in nature and can be costly, so significant performance gains can be made by tightly controlling memory usage and focusing on parallel computation.  This means something like the following expression is not allowed.

    `for (i in 1:10) my_vec <- 1:i`
  
- New variables cannot be declared inside compiled expressions.  This is largely an extension of the above restriction, as it is assumed out of convenience that all variables included in expresions passed to GPUrun for execution already exist in the provided R environment.  The relevant dimesions will be copied from R to the compiled .so lib each time any compiled commands are executed.  It does not matter if variables included in expressions exist in any R environment when compiling, as no data transfer between R and the .so lib occurs until execution.

- Hardware limitations for GPU computation will vary  The defined constant MAX_EVALS_PER_THREAD determines the size of a \_\_shared\_\_ array used for temporary storage of evaluations.  The size of the array is equal to MAX_EVALS_PER_THREAD * THREADS_PER_BLOCK * sizeof(double).  This constant can be adjusted up or down to accomodate the amount of \_\_shared\_\_ memory available.  A separate \_\_shared\_\_ array named linalg_vec is dependentent purely on the constant MAX_LINALG_DIM * sizeof(double), which can also be adjusted.  Current implementations limit mvrnorm() such that the covariance matrix does not have dimension greater than MAX_LINALG_DIM, and any matrix passed to solve() does not have dimension greater than MAX_EVALS_PER_THREAD * THREADS_PER_BLOCK.  Global dynamic memory on a GPU is also generally more limited than that available to the CPU, and all data included in any expression is copied to GPU global memory as doubles.

### Implemented functions
- Elementwise basic math, specifically, `+, -, *, /, %%` implemented as following R's rules for dimension mismatching
- Parentheses, `( )`
- Multiple expression run, `{ }`
- Range function, `:`
- Matrix multiplication and matrix transpose, `%*%, t()`
- Matrix inverse, `solve()`, however this function is only implemented for finding the inverse of the first argument, not for the general case of a %*% x = b.
- Random sampling, `rnorm(), runif(), rtruncnorm(), mvrnorm()`, all arguments must be specificed explicitly, default arguments not currently implemented.
- Elementwise if/else, `ifelse()`.
- Indexing for both reading and writing in up to two dimensions, `[]`, note that only global variables can be indexed, not general expressions, so something like this is not allowed `y <- (x + 3)[1]`
- For loop iteration, `for (_ in _)`
