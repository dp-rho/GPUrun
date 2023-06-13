KERNEL_FILE <- "kernel.cu"
KERNEL_START <- "// [[Kernel.start]]"
KERNEL_END <- "// [[Kernel.end]]"
START_FLAG <- -1
END_FLAG <- 1
OTHER_FLAG <- 0

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
  
  # Write the parsed expressions to fill in the kernel code
  lines_to_write <- kernel_lines[1:start_index]
  for (expr in expr_ls) {
    lines_to_write <- append(lines_to_write, write_expr(expr, var_names))
  }
  lines_to_write <- append(lines_to_write, kernel_lines[stop_index:length(kernel_lines)])
  
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


write_expr <- function(expr, var_names) {
  c("  if (thread_index < max_index) {",
    "    double _i1;",
    "    for (int _iter1 = 0; _iter1 < 100000; _iter1++) {",
    "      _i1 = 1 + _iter1;",
    "      gpu_vars[1].data[thread_index] = add(add(gpu_vars[1].data[thread_index % gpu_vars[1].len], 3.14159), dvs(mul(_i1, 0.420), 0.69));",
    "      sync_blocks();",
    "    }",
    "    // UPDATE ITERATING VARIALBLE AS R DOES",
    "    if (thread_index == 1) {",
    "      gpu_vars[0].data[0] = _i1;",
    "    }",
    "  }")
}



# Puts R string in a form that can be passed to C
vectorize_string <- function(input_string) {
  as.vector(strsplit(input_string, ""))[[1]]
}

# Recursive function that converts an input expression
# into a vector of single chars which can be passed to
# the C program and parsed there
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