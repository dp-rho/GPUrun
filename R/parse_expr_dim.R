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
    var_names,
    dim_type = c('num_evals', 'return_size')
) {
  
  # Match args
  dim_type <- match.arg(dim_type)
  
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
      return(list(len=1, rdim=0, cdim=0))
    }
  )
  
  # Base case 3: empty argument
  if (expr_chars == NULL_ARG) {
    return(list(len=0, rdim=0, cdim=0))
  }
  
  # Base case 4: Infinity representation
  if (expr_chars == R_INF) {
    return(list(len=1, rdim=0, cdim=0))
  }
  
  # General case of (fun ...)
  return(parse_fun(expr_chars, var_names, parser='dimension',
                   dim_type=dim_type))
}
