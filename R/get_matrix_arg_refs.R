# The partial names of global constants used in parsing expressions and dimensions
GPU_PARSING <- "GPU"
CPU_PARSING <- "CPU"
RVAR_REF <- "MAPPING"
INT_EVAL_REF <- "INTERMEDIATE_EVAL_MAPPING"

# Identify the argument references of a matrix function call, note that these
# may be either default global Rvar references, or intermediate eval references
# that exist only to store intermediate evaluations between nested matrix
# function calls.  Note that this may also return additional lines of text
# that must be evaluated prior to the current expression
get_matrix_arg_refs <- function(expr_chars, matrix_fun_str, var_names,
                                allocate_intermediate_exprs) {
  
  args_start <- nchar(matrix_fun_str) + 2
  args <- identify_args(substr(expr_chars, args_start, nchar(expr_chars)))
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
      additional_lines <- c(additional_lines,
                            get_intermediate_evaluation(args[i], var_names))
      
      # Identify the correct GPU mapping, whether it is global Rvars or 
      # intermediate evaluations, and also identify correct index to access 
      # the specific Rvar that is required
      gpu_mapping <- eval(parse(text = paste(GPU_PARSING, ref_type, sep = "_")))
      if (!length(index)) index <- g_int_eval_env$count
      
      parsed_args[i] <- get_ref(index, var_mapping = gpu_mapping)
    }
    else {

      # Get the compiled code array name of the selected reference type
      mapping <- eval(parse(text = paste(CPU_PARSING, ref_type, sep = "_")))
      
      # Parse the argument using the identified mapping, in this case the only
      # thing returned from parse_expr will be a Rvar ref with memory type 
      # dependent on the mapping generated in this function
      parsed_args[i] <- parse_expr(args[i], var_names, type = REF_EVAL, 
                                   var_mapping = mapping,
                                   allocate_intermediate_exprs = FALSE)
    }
  }

  return(list(parsed_args=parsed_args, additional_lines=additional_lines))
}