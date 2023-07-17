GPU_MAPPING <- "gpu_vars"
CPU_MAPPING <- "g_vars"
GPU_INTERMEDIATE_EVAL_MAPPING <- "gpu_int_evals"
CPU_INTERMEDIATE_EVAL_MAPPING <- "g_int_evals"

# Returns the Rvar structure representing the identified variable, although
# this may be a global variable that is present in the compiled expressions, 
# or an intermediate evaluation of a matrix function call.
get_ref <- function(var_num, var_mapping = GPU_MAPPING) {
  return(paste0(var_mapping, "[", as.character(var_num - 1), "]"))
}