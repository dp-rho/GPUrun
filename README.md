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
Note that **ALL** arguments must be specified explicitly, default arguments are not currently implemented.

- Elementwise basic math, specifically, `+, -, *, /, %%` implemented as following R's rules for dimension mismatching
- Assignment, `<-`
- Parentheses, `( )`
- Multiple expression run, `{ }`, however unliked native R implementation, there is no return value from this function call
- Range operator, `:`
- Matrix creation, i.e., explicitly converting a vector or matrix to a new matrix with specified dimensions, `matrix()`, byrow and dimnames are not implemented
- Matrix multiplication and matrix transpose, `%*%, t()`, however, there are no implicit transformations from vectors to matrices, so `matrix()` must be called on any vector the user wishes to pass to a function that operates on matrices
- Matrix inverse, `solve()`, however this function is only implemented for finding the inverse of the first argument, not for the general case of a %*% x = b.
- Vectorized random sampling, `rnorm(), runif(), rexp(), rtruncnorm()`
- Multivariate normal sampling, `mvrnorm()`, tol, empirical, and EISPACK not implemented and current implementation is also restrained to n=1 samples
- Elementwise if/else, `ifelse()`
- Indexing for both reading and writing (i.e., assignment) in up to two dimensions, `[]`
- For loop iteration, `for (_ in _) _`

### Compiling process
The compiling process makes use of the pre-existing Rcpp package building functionality.  Rcpp provides a portable and clean interface for compiling .so libs and linking them to an R package.  The compiled functions which have been registered by the successfully built package can then be executed with the `.Call()` function, although Rcpp packages will hide this functionality behind wrapper R functions that handle the `.Call()` interface under the hood.  GPUrun makes use of devtools in combination with Rcpp to build pseudo packages inside the `compile/compiled_commands` directory which are then accessed with machine generated keys.  This method brings with it some overhead in package structure that is unnecessary in exchange for a well maintained and portable method of compiling .cpp and .cu files and linking the resuting .so libs with R sessions.

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

### Basic Use Case
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

### Pratical Example
Consider the model defined with `Y[i] = rbinom(1, 1, pnorm((t(X[i,]) %*% beta)))`, for `i = 1:observation_size`.  We can approximate beta using a Markov Chain Monte Carlo (MCMC) method to draw samples from the posterior distribution using Gibbs sampling.

First, initialize the true Beta, X, and Y values.

```
set.seed(123)
# The function used to generate a Y sample given X and Beta, this could be implemented more
# efficiently, but it does not matter as it is only called once and this is conceptuall clear
generate_sample_y <- function(X, beta) {
  Y <- numeric(observation_size)
  for (i in 1:observation_size) {
    Y[i] <- rbinom(1, 1, pnorm((t(X[i,]) %*% beta)))
  }
  Y
}

# Initialize the true Beta and the values of X and Y sampled
beta <- rnorm(parameter_size, mean=0, sd=3)
X <- mvrnorm(n=observation_size, mu=numeric(parameter_size), Sigma=diag(parameter_size))
Y <- generate_sample_y(X, beta)
```

Now we write R code to sample from the posterior distribution of beta given Z (variable introduced for the Gibbs sampler).

```
# Requires truncnorm package
run_mcmc_parallel <- function(iters, burnin) {
  beta_cur = rnorm(parameter_size)
  beta_chain <- matrix(numeric(iters * parameter_size), ncol=parameter_size)
  
  for (i in 1:iters) {
    
    Xbeta <- X %*% matrix(beta_cur, nrow=parameter_size, ncol=1)
    Z <- ifelse(Y, 
                rtruncnorm(observation_size, a=0, b=Inf, Xbeta, sd=1),
                rtruncnorm(observation_size, a=-Inf, b=0, Xbeta, sd=1))

    S <- solve(t(X) %*% X)
    mu <-  S %*% t(X) %*% Z
    
    beta_cur <- mvrnorm(1, mu, S)
    beta_chain[i,] <- beta_cur 
  }
  
  return(beta_chain[burnin:iters,])
}
```

Any values of `observation_size` and `parameter_size` can be tested using this code, although observation size should be substantially larger (~200x is a safe bet) for estimations that are reasonable accurate. 

We now implement the same code using GPUrun.

```
# The expression used to run the mcmc
mcmc_commands <- substitute(
 for (i in 1:iters) {
    Xbeta <- X %*% matrix(beta_cur, nrow=parameter_size, ncol=1)
    Z <- ifelse(Y, 
                rtruncnorm(observation_size, a=0, b=Inf, Xbeta, sd=1),
                rtruncnorm(observation_size, a=-Inf, b=0, Xbeta, sd=1))
    
    S <- solve(t(X) %*% X)
    mu <-  S %*% t(X) %*% matrix(Z, nrow=observation_size, ncol=1)
    
    beta_cur <- mvrnorm(1, mu, S)
    beta_chain[i,] <- beta_cur
  }
)

# Wrapper function to initialize values
run_mcmc_gpu <- function(iters, burnin, compiled_commands) {
  
  # Init variables by giving them defined sizes
  beta_cur = rnorm(parameter_size)
  beta_chain <- matrix(numeric(iters * parameter_size), ncol=parameter_size)
  Xbeta <- numeric(observation_size)
  Z <- numeric(observation_size)
  mu <- numeric(parameter_size)
  S <- matrix(numeric(parameter_size ^ 2), nrow = parameter_size)
  i <- 0

  GPUrun::run_commands(compiled_commands, environment())
  
  return(beta_chain[burnin:iters,])
}
```

Finally, we test both implementations for speed and accuracy.

```
test_mcmc <- function() {
  
  # Assign number of iterations and burnins
  iters <- 20000
  burnin <- iters / 2
    
  set.seed(123)
  cat("Testing mcmc native R code ... \n")
  start <- proc.time()
  sampled_betas_2 <- run_mcmc_parallel(iters, burnin)
  cat("Native R efficient code time:\n")
  print(proc.time() - start)
  cat(paste0("mean diff estimated vs actual: ", mean(colMeans(sampled_betas_2) - beta), "\n"))
  
  # Compile mcmc commands 
  cat("Compiling commands ...\n")
  start <- proc.time()
  compiled_mcmc <- GPUrun::compile_commands(list(mcmc_commands))
  cat("Compile time:\n")
  print(proc.time() - start)

  # Call compiled .so lib
  set.seed(123)
  cat("Testing mcmc with GPUrun ... \n")
  start <- proc.time()
  sampled_betas_3 <- run_mcmc_gpu(iters, burnin, compiled_mcmc)
  cat("GPUrun execution time:\n")
  print(proc.time() - start)
  cat(paste0("mean diff estimated vs actual: ", mean(colMeans(sampled_betas_3) - beta), "\n"))

}
```

With relatively small dimensions of `observation_size = 1000` and `parameter_size = 5`, an example output is shown below:

```
> test_mcmc()
Testing mcmc native R code ... 
Native R efficient code time:
   user  system elapsed 
  13.99    0.02   14.00 
mean diff estimated vs actual: 0.0502563734918959
Compiling commands ...
Compile time:
   user  system elapsed 
 26.043   4.399  30.322 
Testing mcmc with GPUrun ... 
Launching 22 blocks with 256 threads per block
Maximum concurrent evaluation of 1 evals per thread
GPUrun execution time:
   user  system elapsed 
  9.316   0.028   9.339 
mean diff estimated vs actual: 0.0338725403017393
```

This is a rather unimpressive performance improvement, especially considering the compile time, however, this model's dimensions do not tend to benefit from massively parallel computation.  Consider instead `observation_size = 20000` and `parameter_size = 100` while also increasing the iterations with `iters = 40000`.

```
> test_mcmc()
Testing mcmc native R code ... 
Native R efficient code time:
   user   system  elapsed
6474.362   1.754 6473.959
mean diff estimated vs actual: -0.0294749809954256
Compiling commands ...
Compile time:
   user  system elapsed
 27.553   4.444  32.146
Testing mcmc with GPUrun ...
Launching 22 blocks with 256 threads per block
Maximum concurrent evaluation of 356 evals per thread
GPUrun execution time:
   user  system elapsed   
481.527   0.389 481.606
mean diff estimated vs actual: -0.0242402119556581
```

The compiled commands executed on the GPU now run ~13x faster than the default R implementation.  It is clear that as dimensions increase, the relative performance improvement also increases.
