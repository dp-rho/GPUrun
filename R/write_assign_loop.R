STORE_RESULT <- "evals"
THREADS_PER_BLOCK <- "THREADS_PER_BLOCK"
EVALS_PER_THREAD <- "gpu_evals_per_thread"
SHARED_MEM_INDEX <- "_shared_mem_index"
EVAL_DATA_INDEX <- "_eval_data_index"



write_assign_loop <- function(var_index, eval_expr, var_mapping = GPU_MAPPING) {
  
  # The var len that determines how many evaluation loops are needed
  # and the array that stores the results
  var_ref <- get_ref(var_index, var_mapping =  var_mapping)
  var_len <- paste0(var_ref, ".len")
  store_results <- paste0(STORE_RESULT, "[", SHARED_MEM_INDEX, "]")
  update_results <- paste0(var_ref, ".data[", EVAL_DATA_INDEX, "]")
  eval_index <- "_eval_index"
  
  # The actual lines of code for storing results
  initialize_shared_mem_index <- paste0(paste(SHARED_MEM_INDEX, PARSED_ASSIGN_FUN, THREAD_ID), ";")
  start_loop <- paste0("for (int ", eval_index, " = 0; ", eval_index, " < ", 
                       EVALS_PER_THREAD, "[", as.character(g_expr_env$count - 1), "]; ", 
                       eval_index, "++) {")
  update_data_index <- paste(EVAL_DATA_INDEX, PARSED_ASSIGN_FUN, "grid_size", 
                             "*", eval_index, "+", GRID_ID)
  store_command <- paste(store_results, PARSED_ASSIGN_FUN, eval_expr)
  update_shared_index <- paste(SHARED_MEM_INDEX, "+=", THREADS_PER_BLOCK)
  store_loop <- c(initialize_shared_mem_index, start_loop,
                  paste0(indent_lines(c(update_data_index, store_command, 
                                        update_shared_index)), ";"),
                  "}")
  
  # The actual lines of code for updating the variable with
  # the stored results
  assignment_len_guard <- paste0("if (", EVAL_DATA_INDEX, " >= ", var_len, ") break")
  update_command <- paste(update_results, PARSED_ASSIGN_FUN, store_results)
  update_loop <- c(initialize_shared_mem_index, start_loop,
                   paste0(indent_lines(c(update_data_index, 
                                         assignment_len_guard,
                                         update_command, 
                                         update_shared_index)), ";"),
                   "}")
  
  return(c(store_loop, SYNC_GRID, update_loop))
}