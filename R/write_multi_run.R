
write_multi_run <- function(
    args, 
    var_names, 
    index = c(EVAL_DATA_INDEX, STORAGE_INDEX, DEFAULT_INDEX, LOOP_ITER_VARS), 
    type = c("data", "ref"), 
    var_mapping = c(GPU_MAPPING, CPU_MAPPING, GPU_INTERMEDIATE_EVAL_MAPPING,
                    CPU_INTERMEDIATE_EVAL_MAPPING),
    depth = 0,
    allocate_intermediate_exprs = TRUE
) {
  
  # Parse all args and retrieve list of character vectors, with each element
  # of the vector representing a line of compliled code for that sub expression
  parsed_args <- lapply(args, parse_expr, var_names=var_names, depth=depth, 
                        var_mapping=var_mapping, index=index, 
                        allocate_intermediate_exprs=allocate_intermediate_exprs)
  
  # If there are multiple arguments to run, sync after each command executed
  if (length(parsed_args) > 1) {
    parsed_args <- c(lapply(parsed_args[1:(length(parsed_args) - 1)], append, values=SYNC_GRID),
                     parsed_args[length(parsed_args)])
  }
  return(unlist(parsed_args, recursive = FALSE, use.names = FALSE))
}