#' @title Parse dimensional information of expression
#' 
#' @description
#' Recursive function which identifies the dimensional information, specifically
#' length, number of rows, and number of columns, of the input R expression.
#' 
#' @param expr_chars A character string representing the unparsed expression
#' for which the dimensions are to be identified.
#' @param var_names A character vector that represents the the named R variables
#' included in these commands.
#' 
#' @returns A named list with 3 elements, the element of 'len' representing 
#' the length of the expression, the element of 'rdim' representing the number
#' of rows in the expression, and the element of 'cdim' representing the number
#' of columns in the expression.
#' @examples
#' parse_expr_dim(expr_chars, var_names)
parse_expr_dim <- function(
    expr_chars, 
    var_names
) {
  
  # Base case 1: a variable in the 
  var_index <- which(var_names == expr_chars)
  if (length(var_index) != 0) {
    
    # For expression lengths, we always require the CPU mapping, as dimensions
    # are parsed only once at run time by the CPU before parallel execution
    # on the GPU
    var_ref <- get_ref(var_index, var_mapping = CPU_MAPPING)
    
    # Return the desired dimensional information of the identified Rvar
    return(list(len = paste0(var_ref, FIELD_OF, LEN_TYPE),
                rdim = paste0(var_ref, FIELD_OF, RDIM_TYPE),
                cdim = paste0(var_ref, FIELD_OF, CDIM_TYPE)))
  }
  
  # Base case 2: a numeric constant
  suppressWarnings(
    if (!is.na(as.numeric(expr_chars))) {
      return(list(len = 1, rdim = 0, cdim = 0))
    }
  )
  
  # Assignment operator `<-`
  if (startsWith(expr_chars, RAW_ASSIGN_FUN)) {
    args_start <- nchar(RAW_ASSIGN_FUN) + 2
    args <- identify_args(substr(expr_chars, args_start, nchar(expr_chars)))
    
    # The dimension of an assignment expression is always the dimension of 
    # the variable being written to since we require that variables do 
    # not change size throughout the evaluation to maximize parallel operations
    # and minimize overhead of checking and updating sizes, which is not 
    # generally parallel
    return(parse_expr_dim(args[1], var_names))
  }
  
  # Basic elementwise math function
  math_index <- which(startsWith(expr_chars, RAW_MATH_FUNS))
  if (length(math_index) != 0) {
    args_start <- nchar(RAW_MATH_FUNS[math_index]) + 2
    args <- identify_args(substr(expr_chars, args_start, nchar(expr_chars)))
    
    # Check special case of negative number
    if (length(args) == 1 & math_index == NEGATIVE_INDEX) {
      return(parse_expr_dim(args[1], var_names))
    }
    
    # Default case of op(a, b), with op some elementwise math function
    parsed_args <- lapply(args, parse_expr_dim, var_names = var_names)

    # The dimension of an elementwise math function is always the max of the 
    # arguments' dimensions
    dims <- lapply(DIMS, function(dim_type) {paste0("std::max(", parsed_args[[1]][[dim_type]], 
                                             ", ", parsed_args[[2]][[dim_type]], ")")})
    names(dims) <- DIMS
    
    return(dims)
  }
  
  # Range operator, i.e., ":"
  if (startsWith(expr_chars, RAW_RANGE_FUN)) {
    args_start <- nchar(RAW_RANGE_FUN) + 2
    args <- identify_args(substr(expr_chars, args_start, nchar(expr_chars)))
    
    # The size of a range operation depends on the actual evaluated data
    # of the arguments, for this reason matrix arguments with intermediate 
    # evaluations are currently undefined for the range operation.  Would
    # be possible but tedious to implement a single iteration CPU evaluation
    # that initializes intermediate values, but this seems like a 
    # superfluous feature, (X %*% X):y is not particularly useful or common
    parsed_args <- lapply(args, parse_expr, var_names = var_names, index = DEFAULT_INDEX,
                          allocate_intermediate_exprs = FALSE)
    return(list(len = paste0("std::floor(abs(", parsed_args[2], " - ", parsed_args[1],  ")  + 1)"),
                rdim = 0, cdim = 0))
  }
  
  # Check matrix multiplication function
  if (startsWith(expr_chars, RAW_MAT_MUL_FUN)) {
    
    # Identify the matrix argument references, do not allocate additional
    # intermediate evaluations, those are allocated during top level parsing
    # calls of parse_expr, not dimensional parsing, which occurs after
    parsed_info <- get_matrix_arg_refs(expr_chars, RAW_MAT_MUL_FUN, var_names,
                                       allocate_intermediate_exprs = FALSE)
    parsed_args <- parsed_info$parsed_args
    
    return(list(len = paste0(parsed_args[1], FIELD_OF, RDIM_TYPE, " * ", 
                             parsed_args[2], FIELD_OF, CDIM_TYPE),
                rdim = paste0(parsed_args[1], FIELD_OF, RDIM_TYPE),
                cdim = paste0(parsed_args[2], FIELD_OF, CDIM_TYPE)))
  }
  
  # Check transpose/inverse function
  if (startsWith(expr_chars, RAW_TRANSPOSE_FUN) | 
      startsWith(expr_chars, RAW_INVERSE_FUN)) {
    
    # Identify the matrix argument references, do not allocate additional
    # intermediate evaluations, those are allocated during top level parsing
    # calls of parse_expr, not dimensional parsing, which occurs after
    parsed_info <- get_matrix_arg_refs(expr_chars, RAW_TRANSPOSE_FUN, var_names,
                                       allocate_intermediate_exprs = FALSE)
    parsed_args <- parsed_info$parsed_args
    
    return(list(len = paste0(parsed_args[1], FIELD_OF, LEN_TYPE),
                rdim = paste0(parsed_args[1], FIELD_OF, CDIM_TYPE),
                cdim = paste0(parsed_args[1], FIELD_OF, RDIM_TYPE)))
  }
}