GPU_MAPPING <- "gpu_vars"
CPU_MAPPING <- "g_vars"
GPU_INTERMEDIATE_EVAL_MAPPING <- "gpu_int_evals"
CPU_INTERMEDIATE_EVAL_MAPPING <- "g_int_evals"

#' @title Gets the referenced Rvar structure in compiled code
#' 
#' @description
#' Retrieves the requested Rvar structure that can be accessible either on the 
#' GPU or CPU, and may represent a global variable or an intermediate evaluation
#' of a nested matrix expression.
#' 
#' @param var_num An integer that represents the index of the Rvar in 
#' one of the global Rvar arrays in compiled code
#' @param var_mapping A character string which controls which
#' type of memory and Rvar is parsed.  The memory may be accessible on either 
#' the GPU or the CPU, but not both, and the Rvar may be part of the global 
#' array that holds all R variables read into memory, or part of the intermediate
#' evaluations array used to store intermediate evaluation matrix arguments.
#' 
#' @returns Character vector representing the compiled code global array
#' and index of the requested Rvar structure
#' @examples
#' get_ref(var_num, var_mapping)
get_ref <- function(
    var_num, 
    var_mapping = c(GPU_MAPPING, CPU_MAPPING, GPU_INTERMEDIATE_EVAL_MAPPING,
                    CPU_INTERMEDIATE_EVAL_MAPPING) 
) {
  return(paste0(var_mapping, "[", as.character(var_num - 1), "]"))
}