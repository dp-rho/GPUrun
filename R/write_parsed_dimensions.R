
#' Writes dimension information stored in a globally accessible environment
#' for either intermediate evaluations, expression lengths, or loop iterations
#' to the input vector of character strings that represents the lines of 
#' code that must be updated in kernel.cu
#' 
#' @param lines_to_edit A character vector representing the lines of code in
#' kernel.cu that will be updated by this function call
#' @param g_var_env A globally accessible environment that stores all of the 
#' relevant parsed expressions and other information necessary to write the
#' machine generated code
#' 
#' @returns Character vector representing the updated lines to be written
#' to kernel.cu
#' @examples
#' write_parsed_dimensions(kernel_lines, g_loop_env)
write_parsed_dimensions <- function(
    lines_to_edit, 
    g_var_env = c(g_int_eval_env, g_loop_env, g_expr_env)
) {
  
  # identify starting and ending line indices based on input flag
  line_indices <- find_start_end_lines(lines_to_edit, g_var_env$flag_str)
  
  # function to generate new lines of machine updated code for each expression
  lambda <- function(i) g_var_env$write_fun(i, g_var_env$exprs_to_write[[i]])
  
  # write expressions based on the saved information g_var_env
  updated_lines <- unlist(lapply(1:g_var_env$count, lambda))
  
  # write the global count of the parsed information
  updated_lines <- c(updated_lines, paste0(g_var_env$count_str, " = ", 
                                           as.character(g_var_env$count), ";"))
  
  # Insert the updated lines into identified indices
  inserted_lines <- c(lines_to_edit[1:line_indices$start],
                      indent_lines(updated_lines),
                      lines_to_edit[line_indices$end:length(lines_to_edit)])
  return(inserted_lines)
}
