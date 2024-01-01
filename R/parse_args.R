#' @title Retrieve compiled parsed arguments for an expression
#' 
#' @description
#' Identify the compiled references or data of a function call, note that these
#' may be either default global Rvar references, or intermediate eval references
#' that exist only to store intermediate evaluations inside nested matrix
#' function calls.  Note that this may also return additional lines of text
#' that must be evaluated prior to the current expression
#' 
#' @param fun_str A character string representing the function
#' itself, this is needed only to identify the arguments' starting locations.
#' @param expr_chars A character string representing the unparsed expression
#' for which the argument references are to be retrieved.
#' @param var_names A character vector that represents the the named R variables
#' included in these commands.
#' @param index An integer that determines which index is used to retrieve data
#' @param type A character vector, either "data" or "ref" to determine the 
#' type of argument needed, either a Rvar ref, or data from the .data field of
#' a parsed Rvar.
#' @param var_mapping A character vector that determines which type of Rvar
#' will be parsed, either on CPU or GPU memory, and in either global R variable
#' storage or the intermediate evaluation storage.
#' @param allocate_intermediate_exprs A Boolean indicating whether intermediate
#' evaluations should be allocated and returned as additional lines of code
#' that must be executed prior to the parsed argument references being used.
#' If this is false, it means the call is being used only to parse dimensional
#' information and not as part of the top level parsing call used to generate
#' the actual code in the kernel function.
#' @param input_args A vector of characters that allows input specification of
#' the raw arguments to be parsed. This is used when arguments of the same function
#' are parsed with different settings, as parse_args can be called on separate
#' portions of the arguments with distinct inputs.
parse_args <- function(
    args,
    expr_chars,
    var_names,
    indices = EVAL_DATA_INDEX,
    types = 'data', 
    var_mapping = c(GPU_MAPPING, CPU_MAPPING, GPU_INTERMEDIATE_EVAL_MAPPING,
                    CPU_INTERMEDIATE_EVAL_MAPPING),
    depth = 0,
    allocate_intermediate_exprs = TRUE
) {
  
  # Match args
  var_mapping <- match.arg(var_mapping)
  
  # Extend index/type to all arguments if only one input
  if (length(indices) == 1) {
    indices <- rep(indices, length(args))
  }
  if (length(types) == 1){
    types <- rep(types, length(args))
  }
  
  parsed_args <- c()
  additional_lines <- c()
  for (i in seq_along(args)) {
    
    # Var index has not yet been identified
    var_index <- -1
    mapping <- 'NO_MAPPING'

    # Check if intermediate expressions must be allocated
    if (allocate_intermediate_exprs) {
      
      # Case where the argument is a defined R variable
      gvar_index <- which(args[i] == var_names)
      if (length(gvar_index))  {
        var_index <- gvar_index
        mapping <- GPU_MAPPING
      }

      # If a reference is needed, or if data is needed but the argument
      # does not automatically return data, allocate intermediate expression
      else if (types[i] == 'ref' || 
               length(which(startsWith(args[i], g_fun_env$void_rets_raw)))) {
        intermediate_evaluations <- get_intermediate_evaluation(args[i], 
                                                                var_names)
        additional_lines <- c(additional_lines, intermediate_evaluations)

        var_index <- g_int_eval_env$count
        mapping <- GPU_INTERMEDIATE_EVAL_MAPPING
      }
      
      # If we have not identified a reference, parse the expression for data
      # using parse_expr, this expression must be a non void return function
      if (var_index == -1) {
        parsed_lines <- parse_expr(args[i], var_names=var_names,
                                   index=indices[i], type='data',
                                   var_mapping=var_mapping, 
                                   allocate_intermediate_exprs=TRUE)
        
        # If this expression required intermediate evaluations, store them
        if (length(parsed_lines) > 1) {
          additional_lines <- c(additional_lines, 
                                parsed_lines[1:(length(parsed_lines) - 1)])
        }
        
        parsed_args[i] <- parsed_lines[length(parsed_lines)]
      }
      
      # If reference is identified and type is 'ref' we return the Rvar 
      # structure as a reference
      else if (types[i] == 'ref') {
        parsed_args[i] <- get_ref(var_index, var_mapping=mapping)
      }
      
      # If a reference is identified by type is data, return data from the
      # index specified as an argument
      else {
        parsed_args[i] <- translate_variable(var_index, index=indices[i],
                                             var_mapping=mapping)
      }
    }
    
    # Case where we are only parsing for dimensions
    else {
      parsed_args[i] <- parse_expr(args[i], var_names=var_names, depth=depth,
                                   index=indices[i], type=types[i],
                                   allocate_intermediate_exprs=FALSE)
    }
  }

  return(list(additional_lines=additional_lines, cur_args=parsed_args))
}
