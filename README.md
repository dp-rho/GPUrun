# GPUrun
Rcpp package that utilizes CUDA to enable fully out of the box GPU computation of basic R expressions via compilation and linking of machine generated .cu and .cpp code.  Requires CUDA 9.0+ and Linux OS. 

*Under Development*

## Overview of Implementation and Functionality
GPUrun is designed for high performance execution of R expressions which operate on strictly numerical data of up to two dimensions.  Only functions explicitly listed here can be included in these expressions.  It is relatively easy to implement any new highly parallel function. Implementing functions which are inherently sequential is possible, but it cannot be done efficiently, as all computation is executed on the GPU.  In general, expressions compiled and executed by GPUrun should mimic the functionality of native R implementations, with some notable restrictions.

### Restrictions
- Variables and expressions cannot change size *during* compiled execution.  Dimensions of expressions, and by extension the rules for computation, are parsed only once by the CPU before the CUDA kernel is called. The parsing of dimensions and allocation of data is generally not parallel in nature and can be costly, so significant performance gains can be made by tightly controlling memory usage and focusing on parallel computation.  This means something like the following expression is not allowed.

    `for (i in 1:10) my_vec <- 1:i`
  
- New variables cannot be declared inside compiled expressions.  This is largely an extension of the above restriction, as it is assumed out of convenience that all variables included in expressions passed to GPUrun for execution already exist in the provided R environment.  The relevant dimesions and data will be copied from R to the compiled .so lib each time any compiled commands are executed.  It does not matter if variables included in expressions exist in any R environment when compiling, as no data transfer between R and the .so lib occurs until execution.

- Hardware limitations for GPU computation will vary.  The defined constant `MAX_EVALS_PER_THREAD` determines the size of a \_\_shared\_\_ array used for temporary storage of evaluations.  The size of the array is equal to `MAX_EVALS_PER_THREAD * THREADS_PER_BLOCK * sizeof(double)`.  This constant can be adjusted up or down to accomodate the amount of \_\_shared\_\_ memory available.  Current implementations limit mvrnorm() such that the covariance matrix does not have dimension greater than `MAX_EVALS_PER_THREAD * THREADS_PER_BLOCK / 2`, and any matrix passed to solve() does not have dimension greater than `MAX_EVALS_PER_THREAD * THREADS_PER_BLOCK`.  Global dynamic memory on a GPU is also generally more limited than that available to the CPU, and all data included in any expression is copied to GPU global memory as doubles, as well as some background allocation.

### Implemented functions
- Elementwise basic math, specifically, `+, -, *, /, %%` implemented as following R's rules for dimension mismatching
- Assignment, `<-`
- Parentheses, `( )`
- Multiple expression run, `{ }`
- Range operator, `:`
- Matrix creation, i.e., explicitly converting a vector or matrix to a new matrix with specified dimensions, `matrix()`, both ncol and nrow must be explicitly specified, byrow and dimnames are not supported.
- Matrix multiplication and matrix transpose, `%*%, t()`
- Matrix inverse, `solve()`, however this function is only implemented for finding the inverse of the first argument, not for the general case of a %*% x = b.
- Random sampling, `rnorm(), runif(), rtruncnorm(), mvrnorm()`, all arguments must be specificed explicitly, default arguments not currently implemented, tol, empirical, EISPACK not implemented for mvrnorm, and mvrnorm is artificially constrained for simplicity to n=1 sample for each call.
- Elementwise if/else, `ifelse()`.
- Indexing for both reading and writing (i.e., assignment) in up to two dimensions, `[]`, note that only defined R variables can be indexed, not general expressions, so something like this is not allowed `y <- (x + 3)[1]`, where as this would be `y[1:2, 5:7] <- x[2:3, 1:3]`
- For loop iteration, `for (_ in _) _`

### Compiling process
The compiling process makes use of the pre-existing Rcpp package building functionality.  Rcpp provides a portable and clean interface for compiling .so libs and linking them to an R package.  The compiled functions which have been registered by the successfully built package can then be executed with the `.Call()` function, although Rcpp packages will hide this functionality behind wrapper R functions that handle the `.Call()` interface under the hood.  GPUrun makes use of devtools in combination with Rcpp to build pseudo packages inside the `compile/installed_libs` directory which are then accessed with machine generated keys.  This method brings with it some overhead in package structure that is unnecessary in exchange for a well maintained and portable method of compiling .cpp and .cu files and linking the resuting .so libs with R sessions.

#### Windows Compiling
CUDA is supported on Windows, so why isn't it supported in GPUrun?  Unfortunately, the NVIDIA distributed CUDA compiler (nvcc) is built on MSVC when using Windows, where as it is built on g++ when using Linux.  Rcpp package building uses g++, and objects compiled with MSVC and g++ cannot be linked.  It is theoretically possible, but beyond the scope of this project, to instead rewrite the Rcpp package building process to use MSVC when compiling on Windows.

#### Compile Errors
The Makevars used for automated compiling assumes the following, review each point below:
- The environment variable `CUDA_HOME` is set to the CUDA installation directory such that target binaries and headers can be found either under `$(CUDA_HOME)/lib64` and `$(CUDA_HOME)/include` (these are generally set as symlinks when using CUDA toolkit) or under `$(CUDA_HOME)/targets/$(OS_ARCH)*/lib` and `$(CUDA_HOME)/targets/$(OS_ARCH)*/include`.  If `CUDA_HOME` is not found, `/usr/local/cuda` will be assumed as `CUDA_HOME`.
- Rcpp package is installed under `$(R_LIBS_USER)/Rcpp`, R should automatically set this and it can be viewed in an R session with the following command: `> Sys.getenv('R_LIBS_USER')`
- General R headers (R.h) can be found under `> R.home('include')` in an R session.
- Compute capacity of the GPU that will be used for execution can be found with nvidia-smi, specifically: 

    `$ nvidia-smi --query-gpu=compute_cap --format=csv,noheader`


## Examples
In general, it is only helpful to execute R commands using GPUrun if they are both notably parallel in nature and computation intensive.  The overhead cost of parsing dimensions and copying data to the GPU makes native R execution far faster in the cases of simple commands, and code which is highly sequential in nature will be slower in all cases if passed to GPUrun.  The following is a simple example of compiling and executing code using GPUrun which will likely see performance improvements during the execution step of the code.

```
# The native R function substitute converts R code into an expression object
my_expr <- substitute(
                for (i in 1:niters) 
                    my_vec <- my_vec + (1 / i)
            )

# compile_commands always expects a list of expressions, even with only one expression
compiled_expr_ex <- GPUrun::compile_commands(list(my_expr))

# Even the iterator variable must be initialized with proper dimension,
# however, the data does not matter as it is immediately overwritten
# during the execution of the for loop, as it would be in native R
i <- 0

# Declare the numer of iterations
niters <- 10000

# Initialize my_vec
my_vec <- rnorm(100000)

# The environment from which to copy the R variables must be passed
GPUrun::run_commands(compiled_expr_ex, environment())
```

The one time compile cost is signficant, however, we can use the same compiled object returned from `compile_commands()` any number of times, and we can also change the content and dimension of data passed between executions, effectively allowing the user to write functions that are executed on the GPU.  For example, using our previously compiled expression called `compiled_expr_ex`, we can change the initial vector and number of iterations.

```
# Use different number of iterations
niters <- 500000

# Use different initial values in vector, and different sized vector
my_vec <- 1:500000

# Call the previously compiled commands
GPUrun::run_commands(compiled_expr_ex, environment())
```
