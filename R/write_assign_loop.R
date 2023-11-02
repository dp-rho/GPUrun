STORE_RESULT <- "temp_evals"
STORAGE_INC <- "_storage_inc"
SHARED_ARR <- "shared_arr"
THREADS_PER_BLOCK <- "THREADS_PER_BLOCK"
EVALS_PER_THREAD <- "gpu_evals_per_thread"
STORAGE_INDEX <- "_storage_index"
EVAL_DATA_INDEX <- "_eval_data_index"
EVAL_LOOP_INDEX <- "_eval_index"
GUARD_LEN <- "_guard_len"
TEMP_EVALS <- "temp_evals"

TEMP_RET <- "//[[RET::VAL]]"
TEMP_EVALS <- "//[[TEMP::EVALS]]"

#' @title Writes code to assign an expression's return value
#' 
#' @description
#' Writes machine generated code to store evaluated expression in
#' a temporary shared array until all elements have been evaluated, then 
#' update the indicated Rvar with the evaluated data
#' 
#' @param var_index An integer value indicating which Rvar is to be updated
#' @param eval_expr The expression that will be evaluated and the corresponding 
#' return value will be stored in the indicated Rvar
#' @param var_mapping A character string which controls which
#' type of memory and Rvar is parsed.  The memory may be accessible on either 
#' the GPU or the CPU, but not both, and the Rvar may be part of the global 
#' array that holds all R variables read into memory, or part of the intermediate
#' evaluations array used to store intermediate evaluation matrix arguments.
#' 
#' @returns Character vector representing the machine generated code necessary
#' to evaluate the expression and store the results in the indicated Rvar
#' @examples
#' write_assign_loop(var_index, eval_expr)
write_assign_loop <- function(
    var_index, 
    eval_expr,
    var_mapping = c(GPU_MAPPING, CPU_MAPPING, GPU_INTERMEDIATE_EVAL_MAPPING,
                    CPU_INTERMEDIATE_EVAL_MAPPING),
    guard_len_expr = NULL,
    index_offset_expr = NULL
) {
  # Match args
  var_mapping <- match.arg(var_mapping)
  
  # Identify the correct Rvar structure and associated len value, the var len 
  # determines how many evaluations are needed in each thread
  var_ref <- get_ref(var_index, var_mapping =  var_mapping)
  var_len <- paste0(var_ref, ".len")
  
  # Special case where the function being assigned directly handles
  # the assignment to target memory
  void_index <- which(startsWith(eval_expr, VOID_RET_FUNS))
  if (length(void_index) != 0) {
    cur_evals_per_thread <- paste0(EVALS_PER_THREAD, "[", as.character(g_expr_env$count - 1), "]")
    eval_expr <- gsub(TEMP_RET, paste0(var_ref, ".", "data"), eval_expr, fixed = TRUE)
    eval_expr <- gsub(TEMP_EVALS, cur_evals_per_thread, eval_expr, fixed = TRUE)
    return(paste0(eval_expr, ";"))
  }
  
  # statements to set the initial default state of the storage variables
  set_storage <- c(
    paste(STORE_RESULT, PARSED_ASSIGN_FUN, SHARED_ARR, ";"),
    paste(STORAGE_INDEX, PARSED_ASSIGN_FUN, THREAD_ID, ";"),
    paste(STORAGE_INC, PARSED_ASSIGN_FUN, "THREADS_PER_BLOCK", ";"),
    paste(EVAL_DATA_INDEX, PARSED_ASSIGN_FUN, GRID_ID, ";")
  )
  
  # Evaluations needed per thread 
  needed_evals <- paste0(EVALS_PER_THREAD, "[", as.character(g_expr_env$count - 1), "]")
  
  # Update if data is too large
  update_storage  <- c(
    paste0("if (", needed_evals, " > ", "MAX_EVALS_PER_THREAD) {"),
    indent_lines(c(
      paste(STORE_RESULT, PARSED_ASSIGN_FUN, SCRATCH_MEM, ";"),
      paste(STORAGE_INDEX, PARSED_ASSIGN_FUN, GRID_ID, ";"),
      paste(STORAGE_INC, PARSED_ASSIGN_FUN, "grid_size", ";"))),
    "}"
  )
  
  # Only reset the increment variables
  set_increments <- c(
    paste(" ", EVAL_DATA_INDEX, PARSED_ASSIGN_FUN, GRID_ID, ";"),
    paste("if (", needed_evals, ">", "MAX_EVALS_PER_THREAD)", 
          STORAGE_INDEX, PARSED_ASSIGN_FUN, GRID_ID, ";"),
    paste("else ", STORAGE_INDEX, PARSED_ASSIGN_FUN, THREAD_ID, ";")
  )


  
  
  # The __shared__ memory array that stores evaluated results while the current
  # thread evaluates other indices, this allows efficient intermediate storage
  # of evaluated results and by extension allows concurrent evaluation of data
  # with much larger dimension than the number of threads. If data is too large
  # instead use global memory storage.
  store_results <- paste0(STORE_RESULT, "[", STORAGE_INDEX, "]")
  
  # The Rvar structure that will have its data field updated at the unique
  # data evaluation index determined by the block, thread, and evaluation loop
  update_results <- paste0(var_ref, ".data[", EVAL_DATA_INDEX, "]")
  
  # Special case to use the index offset expression (already parsed) instead
  if (!is.null(index_offset_expr)) {
    update_results <- paste0(var_ref, ".data[", index_offset_expr, "]")
  }
  
  # initialize the index that will iterate over the __shared__ memory array,
  # this is initialized as the thread index (from 0 - 255) of the current block
  initialize_shared_mem_index <- paste0(paste(STORAGE_INDEX, PARSED_ASSIGN_FUN, THREAD_ID), ";")
  
  # The line of code that creates the evaluation loop scope, this iterates 
  # based on the len of the data and by extension the number of evaluations needed
  # per thread, assuming 256 threads per block, with one block per SM, utilizing
  # all SMs available on the GPU
  start_loop <- paste0("for (int ", EVAL_LOOP_INDEX, " = 0; ", EVAL_LOOP_INDEX, " < ", 
                       needed_evals, "; ", EVAL_LOOP_INDEX, "++) {")
  
  # Update the data index is the index of the global Rvar structure that is being
  # written to, this index is used to evaluate the expression being assigned,
  # as the expression's return value depends on the data index which is being
  # evaluated according to R's rules for mismatched lens where relevant
  update_data_index <- paste(EVAL_DATA_INDEX, "+=", "grid_size")
  
  # Stores the evaluated data in the __shared__ memory array while continued
  # evaluations take place
  store_command <- paste(store_results, PARSED_ASSIGN_FUN, eval_expr)
  
  # Update the __shared__ memory index, which is different than the global 
  # data index, as the __shared__ memory index is repeated by the threads with
  # equal thread index over different blocks
  update_storage_index <- paste(STORAGE_INDEX, "+=", STORAGE_INC)
  
  # The combined lines of code necessary to evaluate and store the results in
  # the intermediate __shared__ memory array
  store_loop <- c(
    start_loop, paste0(indent_lines(c(
      store_command, 
      update_storage_index,
      update_data_index
    )), ";"),
    "}"
  )
  
  # Initialize the guard_len variable used to control overflow
  init_guard_len <- paste(GUARD_LEN, PARSED_ASSIGN_FUN, var_len, ";")
  
  # Special case where an index assignment uses an already parsed expression
  if (!is.null(guard_len_expr)) {
    init_guard_len <- paste(GUARD_LEN, PARSED_ASSIGN_FUN, guard_len_expr, ";")
  }
  
  # To avoid memory errors, do not assign values past the selected Rvar's len
  assignment_len_guard <- paste0("if (", EVAL_DATA_INDEX, " >= ", GUARD_LEN, ") break")
  
  # Update the global Rvar structure with a value from the __shared__ memory 
  update_command <- paste(update_results, PARSED_ASSIGN_FUN, store_results)
  
  # Create the code for the loop that updates the global Rvar using the values
  # stored in the __shared__memory array
  update_loop <- c(
    set_increments, 
    init_guard_len,
    start_loop, paste0(indent_lines(c(
      assignment_len_guard, 
      update_command, 
      update_storage_index,
      update_data_index
    )), ";"),
    "}"
    )
  
  # Full assignment expressions combined together to one vector
  full_exprs <- c(
    set_storage,
    update_storage,
    store_loop,
    SYNC_GRID,
    update_loop
  )

  return(full_exprs)
}