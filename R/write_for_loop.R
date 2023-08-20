#' @title Writes code to execute a for loop
#' 
#' @description
#' Writes compiled code necessary to execute a for loop, first by updating the
#' iteration variable identified as the first argument in the R expression to
#' be equal to the ith value of the second argument on iteration i, then 
#' executes the 3rd argument at each iteration after that update has completed.
#' 
#' @param args A character vector representing the arguments provided in the
#' parsed R expression. The first is the iteration variable that will be 
#' updated at each iteration.  The second is the iterable object that will be
#' used to update the iteration variable at each step.  The third is the 
#' expression to be executed at each iteration.
#' @param var_names A character vector that represents the the named R variables
#' included in these compiled command.
#' @param depth A integer representing the depth of this loop, as nested loops
#' are allowed.  This will be 0 on a top level call to write_for_loop, and 
#' increase by 1 for each nested call.
#' 
#' @returns Character vector representing the lines of code needed to execute
#' the for loop expression on the kernel.
#' @examples
#' write_for_loop(args, var_names, depth)
write_for_loop <- function(
    args, 
    var_names, 
    depth
) {
  
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
  save_dim_info(args[2], g_loop_env)
  
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