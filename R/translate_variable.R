#' @title Translate the inputs to a specific index of a compiled Rvar data
#' 
#' @description
#' Use the variable id number, a modulus Boolean, the index to retrieve
#' and the type of Rvar being requested to create compiled code that will return
#' a double value retrieved from the Rvar data specified by input arguments.
#' 
#' @param var_num An integer representing the Rvar index in one of the compiled
#' global Rvar arrays.
#' @param mod_len A Boolean indicating whether modulus needs to be applied
#' on the data index to prevent array overflow in the compiled data
#' @param index An integer representing the index of data to be retrieved
#' @param var_mapping A character string representing the name of the compiled
#' global array in which the Rvar will be identified
#' 
#' @returns A character string representing the compiled code necessary to
#' retrieve the specified index from the specified Rvar's data.
#' @examples
#' translate_variable(var_num, mod_len, index, var_mapping)
translate_variable <- function(
    var_num, 
    mod_len = TRUE, 
    index = c(EVAL_DATA_INDEX, STORAGE_INDEX, DEFAULT_INDEX, LOOP_ITER_VARS),
    var_mapping = c(GPU_MAPPING, CPU_MAPPING, GPU_INTERMEDIATE_EVAL_MAPPING,
                    CPU_INTERMEDIATE_EVAL_MAPPING)
) {
  
  # Match args
  index <- match.arg(index)
  var_mapping <- match.arg(var_mapping)
  
  # If we are using the default data index, which is 0, no need for modulus 
  if (index == DEFAULT_INDEX) { 
    mod_len <- FALSE
  }
  
  # Retrieve the Rvar reference, i.e., g_vars[0]
  var_struct <- get_ref(var_num, var_mapping)
  
  # Construct the compiled code to retrieve the specified index
  if (mod_len) {
    return(paste0(var_struct, ".", DATA_FIELD, "[", 
                  index, " % ", var_struct, ".", DATA_LENGTH, "]"))
  }
  return(paste0(var_struct, ".", DATA_FIELD, "[", index, "]"))
}