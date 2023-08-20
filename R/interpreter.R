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
