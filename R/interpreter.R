KERNEL_FILE <- "kernel.cu"
DATA_FIELD <- "data"
GRID_ID <- "grid_index"
THREAD_ID <- "thread_index"
DATA_LENGTH <- "len"
SYNC_GRID <- "grid.sync();"
DEFAULT_INDEX <- "DEFAULT_DATA_INDEX"


#' @title Parse input expressions and write code to the kernel
#' 
#' @description
#' Top level function used to parse and interpret the expressions that need
#' to be translated to CUDA code, as well as write the machine generated code
#' to the kernel.
#' 
#' @param expr_ls A list of R expressions to be parsed, interpreted, and 
#' then written to the kernel.
#' @param var_names A character vector that represents the the named R variables
#' included in these commands.
#' 
#' @returns NULL
#' @examples
#' interpreter(expr_ls, var_names)
interpreter <- function(expr_ls, var_names) {
  
  # Get location of .json data and load function meta info
  fun_info_path <- system.file("data", package = "GPUrun")
  function_json_data <- file.path(fun_info_path, "function_data.json")
  dimension_json_data <- file.path(fun_info_path, "dimension_data.json")
  g_fun_env$fun_dict <- fromJSON(function_json_data)
  g_fun_env$void_rets_raw <- g_fun_env$fun_dict[['void_returns_raw']]
  g_fun_env$void_rets_parsed <- g_fun_env$fun_dict[['void_returns_parsed']]
  g_fun_env$dim_dict <- fromJSON(dimension_json_data)
  
  # get location of compile directory
  compile_path <- system.file("compile", package = "GPUrun")
  
  # get location of kernel file
  kernel_path <- file.path(compile_path, PSEUDO_NAME, "src", KERNEL_FILE)
  
  # read the kernel file
  kernel_lines <- base::readLines(kernel_path)

  # identify starting and stopping index of kernel text location
  line_indices <- find_start_end_lines(kernel_lines, flag_str = "Kernel")

  # Initialize globally tracked interpreter variables for storing and 
  # later writing dimensional information
  lapply(c('linalg', 'loop', 'assign', 'int_eval'), init_interpreter, 
         var_names = var_names)
  
  # Write the parsed expressions to fill in the kernel code
  lines_to_write <- c(kernel_lines[1:line_indices$start])
  for (i in seq_along(expr_ls)) {
    comment_delimeter <- paste0("/* Expression ", as.character(i), " code below */")
    text_of_expr <- c(comment_delimeter, write_expr(expr_ls[[i]], var_names), SYNC_GRID)
    text_of_expr <- set_mem_type(text_of_expr, 'gpu')
    lines_to_write <- c(lines_to_write, indent_lines(text_of_expr))
  }
  
  post_kernel_lines <- kernel_lines[line_indices$end:length(kernel_lines)]
  
  post_kernel_lines <- write_dims_to_kernel(post_kernel_lines)

  # Combine all lines to write to output
  lines_to_write <- c(lines_to_write, post_kernel_lines)
  
  # write the updated lines to the kernel file
  base::writeLines(lines_to_write, kernel_path)
  
  return(NULL)
}
