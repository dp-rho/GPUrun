KERNEL_FILE <- "kernel.cu"
KERNEL_START <- "// [[Kernel.start]]"
KERNEL_END <- "// [[Kernel.end]]"
START_FLAG <- -1
END_FLAG <- 1
OTHER_FLAG <- 0
GPU_MAPPING <- "gpu_vars"
DATA_FIELD <- "data"
DATA_ID <- "data_index"
THREAD_ID <- "thread_index"
DEFAULT_INDEX <- "DEFAULT_DATA_INDEX"
DATA_LENGTH <- "len"
DELIM <- " "
OPEN_EXPR <- "("
CLOSE_EXPR <- ")"
SYNC_GRID <- "grid.sync();"
SHARED_MEM_INDEX <- "_shared_mem_index"
EVAL_DATA_INDEX <- "_eval_data_index"

DEFAULT_DEPTH <- 2

RAW_MATH_FUNS <- paste0(OPEN_EXPR, c("+", "-", "*", "/"))
PARSED_MATH_FUNS <- c("add", "sub", "mul", "dvs")

RAW_ASSIGN_FUN <- paste0(OPEN_EXPR, "<-")
PARSED_ASSIGN_FUN <- "="

RAW_FOR_FUN <- paste0(OPEN_EXPR, "for")
PARSED_FOR_FUN <- "for"

STORE_RESULT <- "evals";
THREADS_PER_BLOCK <- "THREADS_PER_BLOCK"
EVALS_PER_THREAD <- "evals_per_thread"

# Writes kernel code to the relevant section of kernel.cu
write_kernel <- function(expr_ls, var_names) {
  # get location of compile directory
  compile_path <- system.file("compile", package = "GPUrun")
  
  # get location of kernel file
  kernel_path <- file.path(compile_path, PSEUDO_NAME, "src", KERNEL_FILE)
  
  # read the kernel file
  kernel_lines <- base::readLines(kernel_path)

  # identify starting and stopping index of kernel text location
  mapped_matches <- as.vector(lapply(kernel_lines, kernel_match))
  start_index <- which(mapped_matches == START_FLAG)
  stop_index <- which(mapped_matches == END_FLAG)

  # Initialize the count of loops in the current commands
  g_loop_count <- 0
  
  # Write the parsed expressions to fill in the kernel code
  lines_to_write <- c(kernel_lines[1:start_index],
                      indent_lines(paste0("if (", DATA_ID, " < max_index) {"), DEFAULT_DEPTH - 1))
  for (expr in expr_ls) {
    lines_to_write <- c(lines_to_write, indent_lines(write_expr(expr, var_names), DEFAULT_DEPTH),
                        indent_lines(SYNC_GRID, DEFAULT_DEPTH))
  }
  lines_to_write <- c(lines_to_write, indent_lines("}", DEFAULT_DEPTH - 1),
                      kernel_lines[stop_index:length(kernel_lines)])
  
  # write the updated lines to the kernel file
  base::writeLines(lines_to_write, kernel_path)
}

kernel_match <- function(input_str) {
  if (grepl(KERNEL_START, input_str, fixed = TRUE)) {
    return(START_FLAG)
  }
  if (grepl(KERNEL_END, input_str, fixed = TRUE)) {
    return(END_FLAG)
  }
  return(OTHER_FLAG);
}

indent_lines <- function(lines, depth) {
  indent <- paste(rep("  ", depth), collapse = "")
  return(paste0(indent, lines))
}

write_expr <- function(expr, var_names) {
  expr_char_vec <- paste0(racket_char_vec(expr), collapse = "")
  parsed_lines <- parse_expr(expr_char_vec, var_names, 0)
  final_lines <- paste0(parsed_lines[length(parsed_lines)], ";")
  if (length(parsed_lines) > 1) {
    final_lines <- c(parsed_lines[1:(length(parsed_lines) - 1)],
                     final_lines)
  }
  return(final_lines)
}

# Recursive function that takes a character vector and parses the vector into
# a single character string that can be written to .cu kernel file
parse_expr <- function(expr_char_vec, var_names, depth, index = EVAL_DATA_INDEX) {
  
  # Base case 1: a variable in the 
  var_index <- which(var_names == expr_char_vec)
  if (length(var_index) != 0) {
    return(translate_variable(var_index, index = index))
  }
  
  # Base case 2: a numeric constant
  suppressWarnings(if (!is.na(as.numeric(expr_char_vec))) {
                     return(expr_char_vec)
                   }
  )
  
  # General case: The form of (fun ...)
  
  # Check basic math functions
  math_index <- which(startsWith(expr_char_vec, RAW_MATH_FUNS) == TRUE)
  if (length(math_index) != 0) {
    args_start <- nchar(RAW_MATH_FUNS[math_index]) + 2
    args <- identify_args(substr(expr_char_vec, args_start, nchar(expr_char_vec)))
    return(paste0(PARSED_MATH_FUNS[math_index], "(", parse_expr(args[1], var_names,
                                                                depth, index),
                  ", ", parse_expr(args[2], var_names, depth, index), ")"))
  }
  
  # Check assignment function
  if (startsWith(expr_char_vec, RAW_ASSIGN_FUN)) {
    args_start <- nchar(RAW_ASSIGN_FUN) + 2
    args <- identify_args(substr(expr_char_vec, args_start, nchar(expr_char_vec)))
    var_index <- which(var_names == args[1])
    
    # Each thread can evaluate up to 22 indices in the expression, this is limited
    # by the __shared__ memory available to store the results of the evaluations 
    # before writing them to global memory associated with the first argument
    # of this expression
    eval_expr <- parse_expr(args[2], var_names, depth)
    return(write_assign_loop(var_index, eval_expr))
  }
  
  # Check for loop function
  if (startsWith(expr_char_vec, RAW_FOR_FUN)) {
    args_start <- nchar(RAW_FOR_FUN) + 2
    args <- identify_args(substr(expr_char_vec, args_start, nchar(expr_char_vec)))
    return(write_for_loop(args, var_names, depth, index))
  }
}

write_assign_loop <- function(var_index, eval_expr) {
  
  # The var len that determines how many evaluation loops are needed
  # and the array that stores the results
  var_len <- paste0("gpu_vars[", as.character(var_index - 1), "].len")
  store_results <- paste0(STORE_RESULT, "[", SHARED_MEM_INDEX, "]")
  update_results <- paste0("gpu_vars[", as.character(var_index - 1), "].data[",
                           EVAL_DATA_INDEX, "]")
  eval_index <- "_eval_index"
  
  # The actual lines of code for storing results
  initialize_SHARED_MEM_INDEX <- paste0(paste(SHARED_MEM_INDEX, PARSED_ASSIGN_FUN, THREAD_ID), ";")
  start_loop <- paste0("for (int ", eval_index, " = 0; ", eval_index, " < ", 
                       EVALS_PER_THREAD, "; ", eval_index, "++) {")
  update_data_index <- paste(EVAL_DATA_INDEX, PARSED_ASSIGN_FUN, "grid_size", 
                             "*", eval_index, "+", DATA_ID)
  store_command <- paste(store_results, PARSED_ASSIGN_FUN, eval_expr)
  update_shared_index <- paste(SHARED_MEM_INDEX, "+=", THREADS_PER_BLOCK)
  store_loop <- c(initialize_SHARED_MEM_INDEX, start_loop,
                  paste0(indent_lines(c(update_data_index, store_command, 
                                        update_shared_index), 1), ";"),
                  "}")
  
  # The actual lines of code for updating the variable with
  # the stored results
  update_command <- paste(update_results, PARSED_ASSIGN_FUN, store_results)
  update_loop <- c(initialize_SHARED_MEM_INDEX, start_loop,
                   paste0(indent_lines(c(update_data_index, update_command, 
                                         update_shared_index), 1), ";"),
                   "}")
  
  return(c(store_loop, SYNC_GRID, update_loop))
}

# Function which writes compiled code lines to execute a for loop in 
# the syntax of R
write_for_loop <- function(args, var_names, depth, index) {
  
  # The compiled variable name that is used to iterate through the for loop,
  # starts at i1, then i2, then i3, etc, as depth increases (nested loops)
  iter_index <- paste0("i", as.character(depth))

  # The line of code which actually begins the loop, note that 'gpu_iter_lens[x]' is
  # the upper bound of the xth loop, gpu_iter_lens is a __constant__ memory storage array
  # of iteration lengths for all loops identified in these commands.  The lengths
  # are originally identified by cpu code using machine generated expressions
  # and stored in g_iter_lens[x], then copied to __constant__ memory for fast 
  # access on the GPU.
  loop_start_line <- paste0("for (int ", iter_index,  
                            " = 0; ", iter_index, " < gpu_iter_lens[", 
                            as.character(g_loop_count),
                            "]; ", iter_index, "++) {")

  # Call the function which writes the necessary machine generated expression that
  # can then be called each time the commands are executed to identify the correct
  # iteration loop length
  write_iter_loop_expr(g_loop_count, args[2], var_names)
  
  # The R variable that will be updated at each iteration to the evaluation of 
  # the second argument in the for expression at the index matching the current iteration
  var_index <- which(var_names == args[1])

  # Only the first thread of the entire grid needs to update the iteration variable,
  # as the iteration variable in an R for loop has a single element by defintion
  limit_line <- "if (data_index == 0) {"

  # The identified iteration variable is updated at DEFAULT_DATA_INDEX 
  # which is 0 in compiled code and 1 in R code.  
  update_iter_lines <- paste(translate_variable(var_index, mod_len = FALSE,
                                               index = DEFAULT_INDEX), 
                             PARSED_ASSIGN_FUN, parse_expr(args[2], var_names,
                                                           depth, index = iter_index), ";")

  # We must sync the entire grid after updating the iteration variable, as other threads
  # which do not need to update the iteration variable could use the old value on this 
  # iteration without the synchronization
  update_iter_lines <- c(limit_line, indent_lines(update_iter_lines, 1), "}", SYNC_GRID)
  
  # Recursive call to parse the expression (can actually be multiple expressions using '{' function) 
  # executed by this for loop
  execute_line <- parse_expr(args[3], var_names, depth + 1, index)

  # The full lines of text for the parsed loop, includes the update to the iteration variable,
  # the execution of the body expression(s), and a syncrhonization command to ensure that
  # the full body has been completed by all threads before the next iteration is started
  body_text <- indent_lines(c(update_iter_lines, execute_line, SYNC_GRID))
  g_loop_count <- g_loop_count + 1
  return(c(loop_start_line, indent_lines(body_text, depth = 1), "}"))
}

# Function which writes machine generated expressions to initialize the compiled values
# for the iteration loop lengths, the raw numeric value may change on separate executions,
# so it is necessary to evaluate the written expression at each call to update the 
# length of iterations for each loop included in the commands
write_iter_loop_expr <- function(g_loop_count, args[2], var_names) {
  
  # NOT IMPLEMENTED
}

identify_arg <- function(expr_char_vec) {
  open_count <- 0
  for (index in 1:nchar(expr_char_vec)) {
    char_at <- substr(expr_char_vec, index, index)
    if (char_at == DELIM & open_count == 0) {
      return(index - 1)
    }
    else if (char_at == OPEN_EXPR) {
      open_count <- open_count + 1
    }
    else if (char_at == CLOSE_EXPR) {
      if (open_count == 0) {
        return(index - 1)
      }
      open_count <- open_count - 1
    }
  }
}

translate_variable <- function(var_num, mod_len = TRUE, index = DATA_ID) {
  var_struct <- paste0(GPU_MAPPING, "[", as.character(var_num - 1), "]")
  if (mod_len) {
    return(paste0(var_struct, ".", DATA_FIELD, "[", 
                  index, " % ", var_struct, ".", DATA_LENGTH, "]"))
  }
  return(paste0(var_struct, ".", DATA_FIELD, "[", index, "]"))
}

identify_args <- function(expr_char_vec) {
  cur_pos <- 1
  args <- c()
  while (cur_pos < nchar(expr_char_vec)) {
    arg_len <- identify_arg(substr(expr_char_vec, cur_pos, nchar(expr_char_vec)))
    arg_chars <- substr(expr_char_vec, cur_pos, cur_pos + arg_len - 1)
    args <- c(args, arg_chars)
    cur_pos <- cur_pos + arg_len + 1
  }
  return(args)
}

# Puts R string in a form that can be passed to C
vectorize_string <- function(input_string) {
  as.vector(strsplit(input_string, ""))[[1]]
}

# Recursive function that converts an input expression
# into a vector of single chars which can be parsed
# in the R code to write machine generated code
# to replicate the native R execution on the GPU
racket_char_vec <- function(input_expr) {
  if (is.numeric(input_expr)) {
    return(vectorize_string(deparse(input_expr)))
  }
  else if (is.symbol(input_expr)) {
    return(vectorize_string(as.character(input_expr)))
  }
  else if (is.function(eval(input_expr[[1]]))) {
    function_string <- vectorize_string(as.character(input_expr))
    if (function_string[1] == "(") function_string <- c('p', 'a', 'r')
    expr_char_vec <- c("(", function_string)
    for (i in 2:length(input_expr)) {
      expr_char_vec <- append(append(expr_char_vec, c(" ")),
                              racket_char_vec(input_expr[[i]]))
    }
    return(append(expr_char_vec, ")"))
  }
  print("ERROR: Expression unmatched ")
  print(as.character(input_expr))
  return(NULL)
}
