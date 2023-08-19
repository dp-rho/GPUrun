KERNEL_FILE <- "kernel.cu"
DATA_FIELD <- "data"
GRID_ID <- "grid_index"
THREAD_ID <- "thread_index"
DATA_LENGTH <- "len"
SYNC_GRID <- "grid.sync();"
DEFAULT_INDEX <- "DEFAULT_DATA_INDEX"


# Writes kernel code to the relevant section of kernel.cu
write_kernel <- function(expr_ls, var_names) {
  # get location of compile directory
  compile_path <- system.file("compile", package = "GPUrun")
  
  # get location of kernel file
  kernel_path <- file.path(compile_path, PSEUDO_NAME, "src", KERNEL_FILE)
  
  # read the kernel file
  kernel_lines <- base::readLines(kernel_path)

  # identify starting and stopping index of kernel text location
  line_indices <- find_start_end_lines(kernel_lines, flag_str = "Kernel")

  # Initialize the count of loops in the current commands, and the expressions
  # used to parse the iteration lengths
  init_iter_loop(var_names)
  
  # Initialize the lengths of the exprs in the current commands, and the expressions
  # used to parse the expr lengths
  init_expr_lens(var_names)
  
  # Initialize the global variables used to track intermediate evaluation Rvars
  init_int_evals(var_names)
  
  # Write the parsed expressions to fill in the kernel code
  lines_to_write <- c(kernel_lines[1:line_indices$start])
  for (i in seq_along(expr_ls)) {
    comment_delimeter <- paste0("/* Expression ", as.character(i), " code below */")
    text_of_expr <- c(comment_delimeter, write_expr(expr_ls[[i]], var_names), SYNC_GRID)
    lines_to_write <- c(lines_to_write, indent_lines(text_of_expr))
  }
  
  post_kernel_lines <- kernel_lines[line_indices$end:length(kernel_lines)]
  
  # Write the dimensions of the intermediate evaluation Rvar structures to the
  # compiled initialization function in kernel.cu called initialize_int_evals()
  post_kernel_lines <- write_parsed_dimensions(post_kernel_lines, g_int_eval_env)
  
  # Write the lengths of loop iterations to the compiled initialization function
  # in kernel.cu called initialized_iter_lens()
  post_kernel_lines <- write_parsed_dimensions(post_kernel_lines, g_loop_env)
  
  # Write the expression lengths and by extension the number of evaluations per 
  # thread to the compiled initialization function in kernel.cu called
  # initialize_expr_lens()
  post_kernel_lines <- write_parsed_dimensions(post_kernel_lines, g_expr_env)

  # Combine all lines to write to output
  lines_to_write <- c(lines_to_write, post_kernel_lines)
  
  # write the updated lines to the kernel file
  base::writeLines(lines_to_write, kernel_path)
}


indent_lines <- function(lines, depth = 1) {
  indent <- paste(rep("  ", depth), collapse = "")
  return(paste0(indent, lines))
}

write_expr <- function(expr, var_names) {

  expr_char_vec <- paste0(racket_char_vec(expr), collapse = "")
  parsed_lines <- parse_expr(expr_char_vec, var_names)
  
  # Only add the ";" to the final line, as some expressions
  # will already be multi-line and not require a new ";"
  final_lines <- paste0(parsed_lines[length(parsed_lines)], ";")
  if (length(parsed_lines) > 1) {
    final_lines <- c(parsed_lines[1:(length(parsed_lines) - 1)],
                     final_lines)
  }
  
  return(final_lines)
}



# Function which writes compiled code lines to execute a for loop in 
# the syntax of R
write_for_loop <- function(args, var_names, depth) {
  
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
                            as.character(g_loop_env$count),
                            "]; ", iter_index, "++) {")

  # Call the function which writes the necessary machine generated expression that
  # can then be called each time the commands are executed to identify the correct
  # iteration loop length
  save_iter_loop_expr(args[2])
  
  # The R variable that will be updated at each iteration to the evaluation of 
  # the second argument in the for expression at the index matching the current iteration
  var_index <- which(var_names == args[1])

  # Only the first thread of the entire grid needs to update the iteration variable,
  # as the iteration variable in an R for loop has a single element by definition
  limit_line <- paste0("if (", GRID_ID," == 0) {")

  # The identified iteration variable is updated at DEFAULT_DATA_INDEX 
  # which is 0 in compiled code and 1 in R code.  
  update_iter_lines <- paste(translate_variable(var_index, mod_len = FALSE,
                                               index = DEFAULT_INDEX), 
                             PARSED_ASSIGN_FUN, parse_expr(args[2], var_names,
                                                           depth = depth, index = iter_index),
                             ";")

  # We must sync the entire grid after updating the iteration variable, as other threads
  # which do not need to update the iteration variable could use the old value on this 
  # iteration without the synchronization
  update_iter_lines <- c(limit_line, indent_lines(update_iter_lines), "}", SYNC_GRID)
  
  # Recursive call to parse the expression (can actually be multiple expressions using '{' function) 
  # executed by this for loop
  execute_line <- parse_expr(args[3], var_names, depth = depth + 1)
  

  # The full lines of text for the parsed loop, includes the update to the iteration variable,
  # the execution of the body expression(s), and a synchronization command to ensure that
  # the full body has been completed by all threads before the next iteration is started
  body_text <- indent_lines(c(update_iter_lines, execute_line, SYNC_GRID))
  return(c(loop_start_line, indent_lines(body_text), "}"))
}

# Function which saves machine generated expressions to initialize the compiled values
# for the iteration loop lengths, the raw numeric value may change on separate executions,
# so it is necessary to evaluate the written expression at each call to update the 
# length of iterations for each loop included in the commands
save_iter_loop_expr <- function(arg) {
  g_loop_env$cur_expr <- arg
  update_expr <- substitute(exprs_to_write[count + 1] <- parse_expr_len(cur_expr, var_names))
  eval(update_expr, envir = g_loop_env)
  assign('count', g_loop_env$count + 1, envir = g_loop_env)
}

# Function which saves machine generated expressions to initialize the compiled values
# for the expression lengths, the raw numeric value may change on separate executions,
# so it is necessary to evaluate the written expression at each call to update the 
# length of expressions in the commands
save_expr_len <- function(arg) {
  g_expr_env$cur_expr <- arg
  update_expr <- substitute(exprs_to_write[count + 1] <- parse_expr_len(cur_expr, var_names))
  eval(update_expr, envir = g_expr_env)
  assign('count', g_expr_env$count + 1, envir = g_expr_env)
}

# Function which saves machine generated expressions to initialize the compiled values
# for the intermediate evaluations, the raw numeric value may change on separate executions,
# so it is necessary to evaluate the written expression at each call to update the 
# length of intermediate evaluations included in the commands
save_int_eval <- function(arg) {
  g_int_eval_env$cur_expr <- arg
  g_int_eval_env$RDIM_TYPE <- RDIM_TYPE
  g_int_eval_env$CDIM_TYPE <- CDIM_TYPE
  update_expr <- substitute(exprs_to_write[[count + 1]]$len <- parse_expr_len(cur_expr, var_names))
  eval(update_expr, envir = g_int_eval_env)
  update_expr <- substitute(exprs_to_write[[count + 1]]$rdim <- parse_expr_len(cur_expr, var_names, type = RDIM_TYPE))
  eval(update_expr, envir = g_int_eval_env)
  update_expr <- substitute(exprs_to_write[[count + 1]]$cdim <- parse_expr_len(cur_expr, var_names, type = CDIM_TYPE))
  eval(update_expr, envir = g_int_eval_env)
  g_int_eval_env$expr_to_eval_map <- append(g_int_eval_env$expr_to_eval_map, arg)
  assign('count', g_int_eval_env$count + 1, envir = g_int_eval_env)
}

translate_variable <- function(var_num, mod_len = TRUE, index = GRID_ID,
                               var_mapping = GPU_MAPPING) {
  if (index == DEFAULT_INDEX) { 
    mod_len <- FALSE
  }
  var_struct <- get_ref(var_num, var_mapping)
  if (mod_len) {
    return(paste0(var_struct, ".", DATA_FIELD, "[", 
                  index, " % ", var_struct, ".", DATA_LENGTH, "]"))
  }
  return(paste0(var_struct, ".", DATA_FIELD, "[", index, "]"))
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
