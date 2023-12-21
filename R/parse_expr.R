OPEN_EXPR <- "("
CLOSE_EXPR <- ")"

R_INF <- "Inf"
C_INF <- "DBL_MAX"

RAW_MATH_FUNS <- paste0(OPEN_EXPR, c("+", "-", "*", "/"))
NEGATIVE_INDEX <- 2
PARSED_MATH_FUNS <- c("add", "sub", "mul", "dvs")

RAW_RANGE_FUN <- paste0(OPEN_EXPR, ":")
PARSED_RANGE_FUN <- "range"

RANDOM_STATE <- "grid_state"

RAW_PAREN_FUN <- paste0(OPEN_EXPR, "par")

RAW_IFELSE_FUN <- paste0(OPEN_EXPR, "ifelse")

RAW_MAT_FUN <- paste0(OPEN_EXPR, "matrix")

RAW_RUNIF_FUN <- paste0(OPEN_EXPR, "runif")
PARSED_RUNIF_FUN <- "runif_device"

RAW_RNORM_FUN <- paste0(OPEN_EXPR, "rnorm")
PARSED_RNORM_FUN <- "rnorm_device"

RAW_REXP_FUN <- paste0(OPEN_EXPR, "rexp")
PARSED_REXP_FUN <- "rexp_device"

RAW_RTRUNC_FUN <- paste0(OPEN_EXPR, "rtruncnorm")
PARSED_RTRUNC_FUN <- "rtruncnorm_device"

RAW_MVRNORM_FUN <- paste0(OPEN_EXPR, "mvrnorm")
PARSED_MVRNORM_FUN <- "mvrnorm_device"

RAW_VEC_RS <- c(RAW_RUNIF_FUN, RAW_RNORM_FUN, RAW_REXP_FUN, RAW_RTRUNC_FUN)
PARSED_VEC_RS <- c(PARSED_RUNIF_FUN, PARSED_RNORM_FUN, PARSED_REXP_FUN, PARSED_RTRUNC_FUN)

RAW_MAT_MUL_FUN <- paste0(OPEN_EXPR, "%*%")
PARSED_MAT_MUL_FUN <- "mat_mul"

RAW_TRANSPOSE_FUN <- paste0(OPEN_EXPR, "t")
PARSED_TRANSPOSE_FUN <- "transpose"

RAW_INVERSE_FUN <- paste0(OPEN_EXPR, "solve")
PARSED_INVERSE_FUN <- "inverse"

RAW_ASSIGN_FUN <- paste0(OPEN_EXPR, "<-")
PARSED_ASSIGN_FUN <- "="

RAW_INDEX_FUN <- paste0(OPEN_EXPR, "[")

RAW_FOR_FUN <- paste0(OPEN_EXPR, "for")
PARSED_FOR_FUN <- "for"

RAW_MULTI_EXPR_FUN <- paste0(OPEN_EXPR, "{")

VOID_RET_FUNS <- c("inverse", "mvrnorm")
RAW_VOID_RET_FUNS <- c(RAW_INVERSE_FUN, RAW_MVRNORM_FUN)

LOOP_ITER_VARS <- paste0("i", 0:9)

#' @title Recursively parse character expression to generate compiled code
#'
#' @description
#' Recursive function that takes a character string and parses the string into
#' a vector of potentially multiple character strings of machine generated 
#' code to be written to kernel.cu. The allocation flag variable determines 
#' whether this is the initial top level call that creates code for the 
#' kernel function, in which case additional intermediate evaluation Rvars will be
#' allocated if matrix arguments are not direct Rvar references. If instead this
#' call is only to parse dimensional information, allocate flag will be FALSE
#' and no additional intermediate evaluations will be allocated.
#' 
#' @param expr_chars A character string that represents the expression currently
#' being parsed.  This expression string will have the general form of 
#' (fun arg1 arg2 ... argn) with any number of args.  
#' @param var_names A character vector with the names of all variables that 
#' are included in the compiled commands.  This is used to identify the index
#' of the Rvar structure in the compiled global array.
#' @param index An integer index of evaluation which, if applicable, determines
#' what index of data is used from an Rvar structure in the compiled memory.
#' @param type A character string which, if applicable, determines whether 
#' an reference to an entire Rvar structure or an index of data in that Rvar
#' structure is parsed.
#' @param var_mapping A character string which controls which
#' type of memory and Rvar is parsed.  The memory may be accessible on either 
#' the GPU or the CPU, but not both, and the Rvar may be part of the global 
#' array that holds all R variables read into memory, or part of the intermediate
#' evaluations array used to store intermediate evaluation matrix arguments.
#' @param depth An integer representing the depth of loop iterations currently
#' being used. This determines the name of the iteration variable used.
#' @param allocate_intermediate_exprs Boolean flag which, if TRUE, results in 
#' allocating additional intermediate evaluation variables in compiled memory
#' to save nested matrix arguments and prevent repeated evaluations through
#' recursion. If FALSE, this call to parse_expr is being used only for 
#' dimension parsing, and so we do not reallocate nested matrix arguments, as
#' dimension parsing occurs after the initial parsing and the allocation is 
#' already complete.
#' 
#' @returns character vector that represents lines of machine written compiled
#' code
#' @examples
#' parse_expr(expr_chars, var_names)
parse_expr <- function(
    expr_chars, 
    var_names, 
    index = c(EVAL_DATA_INDEX, STORAGE_INDEX, DEFAULT_INDEX, LOOP_ITER_VARS), 
    type = c("data", "ref"), 
    var_mapping = c(GPU_MAPPING, CPU_MAPPING, GPU_INTERMEDIATE_EVAL_MAPPING,
                    CPU_INTERMEDIATE_EVAL_MAPPING),
    depth = 0,
    allocate_intermediate_exprs = TRUE
) {

  # Match args
  type <- match.arg(type)
  index <- match.arg(index)
  var_mapping <- match.arg(var_mapping)
  
  # Base case 1: a variable in the compiled commands
  var_index <- which(var_names == expr_chars)
  if (length(var_index)) {
    
    # Evaluation dependent only on a single index of data
    if (type == "data"){
      return(translate_variable(var_index, index=index, 
                                var_mapping=var_mapping))
    }
    
    # Evaluation dependent on the entire Rvar structure
    else {
      return(get_ref(var_index, var_mapping=var_mapping))
    }
  }
  
  # Base case 2: a numeric constant
  suppressWarnings(
    if (!is.na(as.numeric(expr_chars)) & expr_chars != R_INF) {
      return(expr_chars)
    }
  )
  
  # Base case 3: empty argument
  if (expr_chars == NULL_ARG) {
    return(NULL_ARG)
  }
  
  # Base case 4: Infinity representation
  if (expr_chars == R_INF) {
    return(C_INF)
  }
  
  # Base case 5:
  # If the expression is being parsed to identify lengths or dimensions
  # and not to write the kernel and this expression exists as an intermediate
  # evaluation (temporary storage of sub expression evaluation), return the  
  # evaluation Rvar structure with memory access available on either GPU or CPU
  # dependent on the var_mapping, in cases with multiple matches (duplicate
  # intermediate expressions), we can arbitrarily choose the first
  if (expr_chars %in% g_int_eval_env$expr_to_eval_map &
      !allocate_intermediate_exprs) {
    var_index <- which(g_int_eval_env$expr_to_eval_map == expr_chars)
    if (length(var_index > 1)) var_index <- var_index[1]
    return(get_ref(var_index, var_mapping=GPU_INTERMEDIATE_EVAL_MAPPING))
  }
  
  # General case: The form of (fun ...)
  return(parse_fun(expr_chars, var_names, type, index, var_mapping, depth,
                     allocate_intermediate_exprs))
}
