# The partial names of global constants used in parsing expressions and dimensions
GPU_PARSING <- "gpu"
CPU_PARSING <- "g"
RVAR_REF <- "vars"
INT_EVAL_REF <- "int_evals"

#' @title Retrieve argument references for a matrix function
#' 
#' @description
#' Identify the argument references of a matrix function call, note that these
#' may be either default global Rvar references, or intermediate eval references
#' that exist only to store intermediate evaluations inside nested matrix
#' function calls.  Note that this may also return additional lines of text
#' that must be evaluated prior to the current expression
#' 
#' @param expr_chars A character string representing the unparsed expression
#' for which the argument references are to be retrieved.
#' @param matrix_fun_str A character string representing the matrix function
#' itself, this is needed only to identify the arguments' starting locations.
#' @param var_names A character vector that represents the the named R variables
#' included in these commands.
#' @param allocate_intermediate_exprs A Boolean indicating whether intermediate
#' evaluations should be allocated and returned as additional lines of code
#' that must be executed prior to the parsed argument references being used.
#' If this is false, it means the call is being used only to parse dimensional
#' information and not as part of the top level parsing call used to generate
#' the actual code in the kernel function.
#' 
#' @returns List of two character vectors, with named value 'additional_lines'
#' representing the lines of code needed to ensure the intermediate evaluation 
#' arguments are correct, if applicable, and the named value 'parsed_args' 
#' representing the names of the Rvar structures to be used as the arguments
#' to the current matrix expression.
#' @examples
#' get_matrix_arg_refs(expr_chars, matrix_fun_str, var_names, 
#'                     allocate_intermediate_exprs)
get_matrix_arg_refs <- function(expr_chars, matrix_fun_str, var_names,
                                allocate_intermediate_exprs,
                                input_args=NULL) {
  
  if (is.null(input_args)) {
    args_start <- nchar(matrix_fun_str) + 2
    args <- identify_args(substr(expr_chars, args_start, nchar(expr_chars)))
  }
  else args <- input_args
  parsed_args <- c()
  additional_lines <- c()

  for (i in seq_along(args)) {
    
    # If the argument exists as a global Rvar, get reference from global
    # Rvar array, else get reference from the intermediate evaluation array
    index <- which(args[i] == var_names)
    if (length(index)) ref_type <- RVAR_REF
    else ref_type <- INT_EVAL_REF
    
    # Identify whether CPU or GPU memory is requested, GPU parsing is called
    # first, and this is when intermediate allocations are requested, so if
    # no intermediate evaluation parsing is requested, type is CPU, else GPU
    if (allocate_intermediate_exprs) {
      
      intermediate_evaluations <- get_intermediate_evaluation(args[i], var_names)
      additional_lines <- c(additional_lines, intermediate_evaluations)
      
      # Identify the correct GPU mapping, whether it is global Rvars or 
      # intermediate evaluations, and also identify correct index to access 
      # the specific Rvar that is required
      mapping <- paste(GPU_PARSING, ref_type, sep = "_")
      if (!length(index)) index <- g_int_eval_env$count
      
      parsed_args[i] <- get_ref(index, var_mapping = mapping)
    }
    else {

      # Get the compiled code array name of the selected reference type
      mapping <- paste(CPU_PARSING, ref_type, sep = "_")
      
      # Parse the argument using the identified mapping, in this case the only
      # thing returned from parse_expr will be a Rvar ref with memory type 
      # dependent on the mapping generated in this function
      parsed_args[i] <- parse_expr(args[i], var_names, type = "ref", 
                                   var_mapping = mapping,
                                   allocate_intermediate_exprs = FALSE)
    }
  }

  return(list(parsed_args=parsed_args, additional_lines=additional_lines))
}
