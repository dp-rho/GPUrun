DEFAULT_DEPTH <- 1

OPEN_EXPR <- "("
CLOSE_EXPR <- ")"

RAW_MATH_FUNS <- paste0(OPEN_EXPR, c("+", "-", "*", "/"))
NEGATIVE_INDEX <- 2
PARSED_MATH_FUNS <- c("add", "sub", "mul", "dvs")

RAW_RANGE_FUN <- paste0(OPEN_EXPR, ":")
PARSED_RANGE_FUN <- "range"

RAW_MAT_MUL_FUN <- paste0(OPEN_EXPR, "%*%")
PARSED_MAT_MUL_FUN <- "mat_mul"

RAW_TRANSPOSE_FUN <- paste0(OPEN_EXPR, "t")
PARSED_TRANSPOSE_FUN <- "transpose"

RAW_ASSIGN_FUN <- paste0(OPEN_EXPR, "<-")
PARSED_ASSIGN_FUN <- "="

RAW_FOR_FUN <- paste0(OPEN_EXPR, "for")
PARSED_FOR_FUN <- "for"

RAW_MULTI_EXPR_FUN <- paste0(OPEN_EXPR, "{")

STORE_RESULT <- "evals"
THREADS_PER_BLOCK <- "THREADS_PER_BLOCK"
EVALS_PER_THREAD <- "gpu_evals_per_thread"

# Recursive function that takes a character string and parses the vector into
# a vector of potentially multiple character strings of machine generated 
# code to written to kernel.cu. The allocation boolean variable determines 
# whether this is the initial top level call that creates code for the 
# kernel function, in which case additional intermediate evaluations will be
# allocated if matrix arguments are not direct Rvar references. If instead this
# call is only to parse dimensional information, allocate boolean will be FALSE
# and no additional intermediate evaluations will be allocated
parse_expr <- function(expr_chars, var_names, depth = DEFAULT_DEPTH, 
                       index = EVAL_DATA_INDEX, type = DATA_EVAL, 
                       var_mapping = GPU_MAPPING,
                       allocate_intermediate_exprs = TRUE) {
  
  # Base case 1: a variable in the 
  var_index <- which(var_names == expr_chars)
  if (length(var_index) != 0) {
    
    # Evaluation dependent only on a single index of data
    if (type == DATA_EVAL){
      return(translate_variable(var_index, index = index, var_mapping = var_mapping))
    }
    
    # Evaluation dependent on the entire Rvar structure
    else {
      return(get_ref(var_index, var_mapping = var_mapping))
    }
  }
  
  # Base case 2: a numeric constant
  suppressWarnings(
    if (!is.na(as.numeric(expr_chars))) {
      return(expr_chars)
    }
  )
  
  # General case: The form of (fun ...)
  
  # Check basic math functions
  math_index <- which(startsWith(expr_chars, RAW_MATH_FUNS))
  if (length(math_index) != 0) {

    # If the expression is being parsed to identify lengths or dimensions
    # and not to write the kernel, return the intermediate evaluation
    # Rvar structure with memory access available on either GPU or CPU
    # dependent on the var_mapping
    if (expr_chars %in% g_int_eval_env$expr_to_eval_map &
        !allocate_intermediate_exprs) {
      var_index <- which(g_int_eval_env$expr_to_eval_map == expr_chars)
      return(get_ref(var_index, var_mapping = var_mapping))
    }
    
    args_start <- nchar(RAW_MATH_FUNS[math_index]) + 2
    args <- identify_args(substr(expr_chars, args_start, nchar(expr_chars)))
    parsed_args <- lapply(args, parse_expr, var_names = var_names, depth = depth,
                          index = index, allocate_intermediate_exprs = allocate_intermediate_exprs)
    
    # Check special case of negative number
    if (length(parsed_args) == 1 & math_index == NEGATIVE_INDEX) {
      additional_lines <- get_additional_lines(parsed_args)
      parsed_num <- parsed_args[1][length(parsed_args[1])]
      cur_expr <- paste0("-(", parsed_num, ")")
      return(c(additional_lines, cur_expr))
    }
    
    # General case of the form op(arg1, arg2) where op is some element wise math
    additional_lines <- get_additional_lines(parsed_args)
    parsed_num1 <- parsed_args[1][length(parsed_args[1])]
    parsed_num2 <- parsed_args[2][length(parsed_args[2])]
    cur_expr <- paste0(PARSED_MATH_FUNS[math_index], "(", parsed_num1,
                       ", ", parsed_num2, ")")
    return(c(additional_lines, cur_expr))
  }
  
  # Check range function (i.e. ':')
  if (startsWith(expr_chars, RAW_RANGE_FUN)) {
    args_start <- nchar(RAW_RANGE_FUN) + 2
    args <- identify_args(substr(expr_chars, args_start, nchar(expr_chars)))
    parsed_args <- lapply(args, parse_expr, var_names = var_names, depth = depth,
                          index = DEFAULT_INDEX, allocate_intermediate_exprs = allocate_intermediate_exprs)
    additional_lines <- get_additional_lines(parsed_args)
    start <- parsed_args[1][length(parsed_args[1])]
    stop <- parsed_args[2][length(parsed_args[2])]
    cur_expr <- paste0(PARSED_RANGE_FUN, "(", start, ", ", stop, ", ", index, ")")
    return(c(additional_lines, cur_expr))
  }
  
  # Check matrix multiplication function
  if (startsWith(expr_chars, RAW_MAT_MUL_FUN)) {
    return(parse_matrix_expr(expr_chars, RAW_MAT_MUL_FUN, var_names, var_mapping,
                             allocate_intermediate_exprs, PARSED_MAT_MUL_FUN))
  }

  # Check matrix transpose function
  if (startsWith(expr_chars, RAW_TRANSPOSE_FUN)) {
    return(parse_matrix_expr(expr_chars, RAW_TRANSPOSE_FUN, var_names, var_mapping,
                             allocate_intermediate_exprs, PARSED_TRANSPOSE_FUN))
  }
  
  # Check assignment function
  if (startsWith(expr_chars, RAW_ASSIGN_FUN)) {
    args_start <- nchar(RAW_ASSIGN_FUN) + 2
    args <- identify_args(substr(expr_chars, args_start, nchar(expr_chars)))
    var_index <- which(var_names == args[1])
    
    # Each thread can evaluate up to 22 indices in the expression, this is limited
    # by the __shared__ memory available to store the results of the evaluations 
    # before writing them to global memory associated with the first argument
    # of this expression
    eval_expr_lines <- parse_expr(args[2], var_names, depth,
                                  allocate_intermediate_exprs = allocate_intermediate_exprs)
    additional_lines <- get_additional_lines(list(eval_expr_lines))
    eval_expr <- eval_expr_lines[length(eval_expr_lines)]
    save_expr_len(expr_chars)
    assign_lines <- write_assign_loop(var_index, eval_expr)
    return(c(additional_lines, assign_lines))
  }
  
  # Check multiple run function (i.e. '{')
  if (startsWith(expr_chars, RAW_MULTI_EXPR_FUN)) {
    args_start <- nchar(RAW_MULTI_EXPR_FUN) + 2
    args <- identify_args(substr(expr_chars, args_start, nchar(expr_chars)))
    parsed_args <- lapply(args, parse_expr, var_names = var_names, depth = depth,
                          index = index, allocate_intermediate_exprs = allocate_intermediate_exprs)
    parsed_args <- c(lapply(parsed_args[1:(length(parsed_args) - 1)], append, values = SYNC_GRID),
                     parsed_args[length(parsed_args)])
    return(unlist(parsed_args, recursive = FALSE, use.names = FALSE))
  }
  
  # Check for loop function
  if (startsWith(expr_chars, RAW_FOR_FUN)) {
    args_start <- nchar(RAW_FOR_FUN) + 2
    args <- identify_args(substr(expr_chars, args_start, nchar(expr_chars)))
    return(write_for_loop(args, var_names, depth, index))
  }
}