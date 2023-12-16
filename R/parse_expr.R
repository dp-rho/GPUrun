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

RAW_RTRUNC_FUN <- paste0(OPEN_EXPR, "rtruncnorm")
PARSED_RTRUNC_FUN <- "rtruncnorm_device"

RAW_MVRNORM_FUN <- paste0(OPEN_EXPR, "mvrnorm")
PARSED_MVRNORM_FUN <- "mvrnorm_device"

RAW_VEC_RS <- c(RAW_RUNIF_FUN, RAW_RNORM_FUN, RAW_RTRUNC_FUN)
PARSED_VEC_RS <- c(PARSED_RUNIF_FUN, PARSED_RNORM_FUN, PARSED_RTRUNC_FUN)

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
      return(translate_variable(var_index, index=index, var_mapping=var_mapping))
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
  # evaluation (it is used in a matrix function), return the intermediate 
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
  
  # Check basic math functions
  math_index <- which(startsWith(expr_chars, RAW_MATH_FUNS))
  if (length(math_index)) {
    
    # TODO: Apply parse_args fun to other cases for modularity
    parsed_info <- parse_args(RAW_MATH_FUNS[math_index], expr_chars,
                              var_names=var_names, depth=depth, index=index, 
                              type=type, var_mapping=var_mapping, 
                              allocate_intermediate_exprs=allocate_intermediate_exprs)
    additional_lines <- parsed_info$additional_lines
    cur_args <- parsed_info$cur_args
    
    # Check special case of negative number
    if (length(cur_args) == 1 & math_index == NEGATIVE_INDEX) {
      cur_expr <- paste0("-(", cur_args, ")")
      return(c(additional_lines, cur_expr))
    }
    cur_expr <- paste0(PARSED_MATH_FUNS[math_index], "(", cur_args[1],
                       ", ", cur_args[2], ")")
    return(c(additional_lines, cur_expr))
  }
  
  # Check range function (i.e. ':')
  if (startsWith(expr_chars, RAW_RANGE_FUN)) {
    parsed_info <- parse_args(RAW_RANGE_FUN, expr_chars,
                              var_names=var_names, depth=depth, index="DEFAULT_DATA_INDEX", 
                              type=type, var_mapping=var_mapping, 
                              allocate_intermediate_exprs=allocate_intermediate_exprs)
    additional_lines <- parsed_info$additional_lines
    cur_args <- parsed_info$cur_args
    cur_expr <- paste0(PARSED_RANGE_FUN, "(", cur_args[1], ", ", cur_args[2], ", ", index, ")")
    return(c(additional_lines, cur_expr))
  }
  
  # Check vectorized random sampling
  rs_index <- which(startsWith(expr_chars, RAW_VEC_RS))
  if (length(rs_index) != 0) {
    parsed_info <- parse_args(RAW_VEC_RS[rs_index], expr_chars,
                              var_names=var_names, depth=depth, index=index, 
                              type=type, var_mapping=var_mapping, 
                              allocate_intermediate_exprs=allocate_intermediate_exprs)
    additional_lines <- parsed_info$additional_lines
    cur_args <- parsed_info$cur_args
    compiled_args <- paste(cur_args[2:length(cur_args)], collapse=", ")
    cur_expr <- paste0(PARSED_VEC_RS[rs_index], "(", compiled_args, ", ", RANDOM_STATE, ")")
    return(c(additional_lines, cur_expr))
  }

  # Check matrix dimension function
  if (startsWith(expr_chars, RAW_MAT_FUN)) {
    args_start <- nchar(RAW_MAT_FUN) + 2
    args <- identify_args(substr(expr_chars, args_start, nchar(expr_chars)))
    expr_to_use <- parse_expr(args[1], var_names=var_names, depth=depth,
                              index=index, 
                              allocate_intermediate_exprs=allocate_intermediate_exprs)
    dim_exprs <- lapply(args[2:length(args)], parse_expr, var_names=var_names, depth=depth,
                          index="DEFAULT_DATA_INDEX", var_mapping=var_mapping,
                          allocate_intermediate_exprs=allocate_intermediate_exprs)

    additional_lines <- get_additional_lines(append(list(expr_to_use), dim_exprs))
    cur_expr <- expr_to_use[length(expr_to_use)]

    # NOTE: Void return functions (currently only matrix inverse) will not work by simply
    # returning the parsed expression for argument 1, as the void return functions do not
    # directly return a value, rather they write the calculated values to a provided 
    # memory address passed to the compiled function
      
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
  
  # Check matrix inverse function
  if (startsWith(expr_chars, RAW_INVERSE_FUN)) {
    return(parse_matrix_expr(expr_chars, RAW_INVERSE_FUN, var_names, var_mapping,
                             allocate_intermediate_exprs, PARSED_INVERSE_FUN))
  }
  
  # Check matrix inverse function
  if (startsWith(expr_chars, RAW_MVRNORM_FUN)) {
    return(parse_matrix_expr(expr_chars, RAW_MVRNORM_FUN, var_names, var_mapping,
                             allocate_intermediate_exprs, PARSED_MVRNORM_FUN))
  }
  
  # Check assignment function
  if (startsWith(expr_chars, RAW_ASSIGN_FUN)) {
    args_start <- nchar(RAW_ASSIGN_FUN) + 2
    args <- identify_args(substr(expr_chars, args_start, nchar(expr_chars)))
    
    # base case assign, entire variable overwritten
    var_index <- which(var_names == args[1])
    
    # Special case where we assign to a specific set of indices rather
    # than the entire variable
    index_offset_expr <- NULL
    guard_len_expr <- NULL
    additional_lines <- c()
    if (startsWith(args[1], RAW_INDEX_FUN)) {
      sub_args_start <- nchar(RAW_INDEX_FUN) + 2
      sub_args <- identify_args(substr(args[1], sub_args_start, nchar(args[1])))
      
      # First argument must be a global variable
      var_index <- which(var_names == sub_args[1])
      var_struct <- get_ref(var_index, var_mapping = var_mapping)
      
      parsed_sub_args <- lapply(sub_args[2:length(sub_args)], parse_expr, 
                                var_names = var_names, 
                                var_mapping = var_mapping, index = index, 
                                allocate_intermediate_exprs = allocate_intermediate_exprs)
      additional_lines <- get_additional_lines(parsed_sub_args)
      cur_sub_args <- lapply(parsed_sub_args, function(vec) { vec[length(vec)] })
      cur_sub_args <- unlist(cur_sub_args)
      parsed_sub_dims <- lapply(sub_args, parse_expr_dim, 
                                var_names = var_names)
      
      # Parse the offset expression to access each index in parallel
      index_offset_expr <- parse_index_expr(cur_sub_args, 
                                            parsed_sub_dims, 
                                            var_struct)
      guard_len_expr <- set_mem_type(parse_expr_dim(args[1], var_names)$len,
                                     "gpu")
    }
    
    
    # Each thread can evaluate up to 22 indices in the expression, this is limited
    # by the __shared__ memory available to store the results of the evaluations 
    # before writing them to global memory associated with the first argument
    # of this expression

    eval_expr_lines <- parse_expr(args[2], var_names, depth=depth, 
                                  index=index, var_mapping=var_mapping,
                                  allocate_intermediate_exprs=allocate_intermediate_exprs)
    additional_lines <- c(get_additional_lines(list(eval_expr_lines)),
                          additional_lines)
    eval_expr <- eval_expr_lines[length(eval_expr_lines)]
    save_dim_info(expr_chars, g_expr_env)
    assign_lines <- write_assign_loop(var_index, eval_expr, guard_len_expr=guard_len_expr,
                                      index_offset_expr = index_offset_expr)
    return(c(additional_lines, assign_lines))
  }
  
  # Index function (on evaluation, since assignment index expressions
  # are caught in the assignment parsing case)
  if (startsWith(expr_chars, RAW_INDEX_FUN)) {
    args_start <- nchar(RAW_INDEX_FUN) + 2
    args <- identify_args(substr(expr_chars, args_start, nchar(expr_chars)))
    parsed_args <- lapply(args[2:length(args)], parse_expr, var_names=var_names, depth=depth,
                          var_mapping=var_mapping,
                          index=index, allocate_intermediate_exprs=allocate_intermediate_exprs)
      
    additional_lines <- get_additional_lines(parsed_args)
    cur_args <- lapply(parsed_args, function(vec) { vec[length(vec)] })
    cur_args <- unlist(cur_args)
    parsed_dims <- lapply(args, parse_expr_dim, var_names = var_names) 
    
    # First argument must be a global variable
    var_index <- which(var_names == args[1])
    var_struct <- get_ref(var_index, var_mapping = var_mapping)
    
    # Parse the offset expression to access each index in parallel
    index_offset_expr <- parse_index_expr(cur_args, parsed_dims, var_struct)
    
    # The current expression is created with the parsed index offset and the identified Rvar
    cur_expr <- paste0(var_struct, ".data[", index_offset_expr, "]")

    return(c(additional_lines, cur_expr))
  }
  
  # Check multiple run function (i.e. '{')
  if (startsWith(expr_chars, RAW_MULTI_EXPR_FUN)) {
    args_start <- nchar(RAW_MULTI_EXPR_FUN) + 2
    args <- identify_args(substr(expr_chars, args_start, nchar(expr_chars)))
    parsed_args <- lapply(args, parse_expr, var_names=var_names, depth=depth, var_mapping=var_mapping,
                          index=index, allocate_intermediate_exprs=allocate_intermediate_exprs)
    
    # If there are multiple arguments to run, sync after each command executed
    if (length(parsed_args) > 1) {
      parsed_args <- c(lapply(parsed_args[1:(length(parsed_args) - 1)], append, values=SYNC_GRID),
                       parsed_args[length(parsed_args)])
    }
    return(unlist(parsed_args, recursive = FALSE, use.names = FALSE))
  }
  
  # Check for paren function, i.e., '(', special case where we simply
  # parse the first and only argument 
  if (startsWith(expr_chars, RAW_PAREN_FUN)) {
    arg_chars <- substr(expr_chars, nchar(RAW_PAREN_FUN) + 2, 
                        nchar(expr_chars) - 1)
    return(parse_expr(arg_chars, var_names=var_names, depth=depth,
                      index=index, type=type, var_mapping=var_mapping,
                      allocate_intermediate_exprs=allocate_intermediate_exprs))
  }
  
  # Check vectorized ifelse function
  if (startsWith(expr_chars, RAW_IFELSE_FUN)) {
    parsed_info <- parse_args(RAW_IFELSE_FUN, expr_chars,
                              var_names=var_names, depth=depth, index=index, 
                              type=type, var_mapping=var_mapping, 
                              allocate_intermediate_exprs=allocate_intermediate_exprs)
    additional_lines <- parsed_info$additional_lines
    cur_args <- parsed_info$cur_args
    cur_expr <- paste0(cur_args[1], " ? ", cur_args[2], " : ", cur_args[3])
    return(c(additional_lines, cur_expr))
  }
  
  # Check for loop function
  if (startsWith(expr_chars, RAW_FOR_FUN)) {
    args_start <- nchar(RAW_FOR_FUN) + 2
    args <- identify_args(substr(expr_chars, args_start, nchar(expr_chars)))
    return(write_for_loop(args, var_names, depth = depth))
  }
}
