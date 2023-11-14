SCRATCH_MEM <- "gpu_mem.gpu_scratch_memory"

#' @title Recursively parse a matrix function call to generate compiled code
#'
#' @description
#' Matrix specific function that takes a character string and parses the string 
#' into a vector of potentially multiple character strings of machine generated 
#' code to be written to kernel.cu. The allocation flag variable determines 
#' whether this is the initial top level call that creates code for the 
#' kernel function, in which case additional intermediate evaluation Rvars will be
#' allocated if matrix arguments are not direct Rvar references. If instead this
#' call is only to parse dimensional information, allocate flag will be FALSE
#' and no additional intermediate evaluations will be allocated. This function
#' is designed only for matrix function calls.
#' 
#' @param expr_chars A character string that represents the expression currently
#' being parsed.  This expression string will have the general form of 
#' (fun arg1 arg2 ... argn) with any number of args, and fun being some matrix
#' function specifically.  
#' @param var_names A character vector with the names of all variables that 
#' are included in the compiled commands. This is used to identify the index
#' of the Rvar structure in a compiled global array.
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
#' code for the matrix function call
#' @examples 
#' parse_matrix_expr(expr_chars, raw_fun_str, var_names, var_mapping,
#'                   allocate_intermediate_exprs, parsed_fun_str)
parse_matrix_expr <- function(expr_chars, raw_fun_str, var_names, var_mapping,
                              allocate_intermediate_exprs, parsed_fun_str) {

  # If the expression is being parsed to identify lengths or dimensions
  # and not to write the kernel, return the intermediate evaluation
  # Rvar structure with memory access available on either GPU or CPU
  # dependent on the var_mapping
  if (expr_chars %in% g_int_eval_env$expr_to_eval_map &
      !allocate_intermediate_exprs) {
    var_index <- which(g_int_eval_env$expr_to_eval_map == expr_chars)
    return(get_ref(var_index, var_mapping = var_mapping))
  }
  
  # Check special case where function requires taking the return value
  # as an argument
  void_index <- which(startsWith(raw_fun_str, RAW_VOID_RET_FUNS))
  if (length(void_index) != 0) {
    # Case by case creation of void return function call with placeholder
    
    # Inverse matrix
    if (RAW_VOID_RET_FUNS[void_index] == RAW_INVERSE_FUN) {
      parsed_info <- get_matrix_arg_refs(expr_chars, raw_fun_str, var_names,
                                         allocate_intermediate_exprs = allocate_intermediate_exprs)
      additional_lines <- parsed_info$additional_lines
      parsed_args <- parsed_info$parsed_args
      
      compiled_args <- paste(parsed_args[1], TEMP_RET,
                             GRID_ID, TEMP_EVALS, "grid_size", THREAD_ID,
                             SHARED_ARR, "grid", sep = ", ") 
      cur_expr <- paste0(parsed_fun_str, "(", compiled_args, ")")
    }
    
    # Multivariate normal sampling case
    if (RAW_VOID_RET_FUNS[void_index] == RAW_MVRNORM_FUN) {
      
      args_start <- nchar(RAW_MVRNORM_FUN) + 2
      args <- identify_args(substr(expr_chars, args_start, nchar(expr_chars)))
      n <- parse_expr(args[1], var_names, index="DEFAULT_DATA_INDEX",
                      allocate_intermediate_exprs=allocate_intermediate_exprs)

      parsed_info <- get_matrix_arg_refs(expr_chars, raw_fun_str, var_names,
                                         allocate_intermediate_exprs=allocate_intermediate_exprs,
                                         input_args=args[2:length(args)])
      additional_lines <- parsed_info$additional_lines
      parsed_args <- parsed_info$parsed_args
      
      # Save dimension of covariance matrix for linalg memory allocation
      covar_dim <- set_mem_type(paste0(parsed_args[2], ".rdim"), 'cpu')
      assign("linalg_dims", append(g_linalg_env$linalg_dims, covar_dim), 
             g_linalg_env)
      
      mu_data <- paste0(parsed_args[1], ".data")
      
      compiled_args <- paste(mu_data, parsed_args[2], TEMP_RET, SHARED_ARR,
                             "linalg_vec", "grid_size", GRID_ID,
                             THREAD_ID, TEMP_EVALS, "grid",
                             RANDOM_STATE, sep = ", ")
      cur_expr <- paste0(parsed_fun_str, "(", compiled_args, ")")
    }
  }
  else {
    parsed_info <- get_matrix_arg_refs(expr_chars, raw_fun_str, var_names,
                                       allocate_intermediate_exprs=allocate_intermediate_exprs)
    additional_lines <- parsed_info$additional_lines
    parsed_args <- parsed_info$parsed_args
    
    compiled_args <- paste(c(parsed_args, EVAL_DATA_INDEX), collapse = ", ")
    cur_expr <- paste0(parsed_fun_str, "(", compiled_args, ")")
  }
  
  return(c(additional_lines, cur_expr))
}