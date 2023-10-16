#' @title Writes code to evaluate and save nested matrix argument
#' 
#' @description
#' Writes compiled code necessary to execute a nested matrix function, then save
#' the results of that expression into an intermediate evaluation Rvar that will
#' be retrieved as the argument of the parent expression calling this function.
#' 
#' @param arg A character string representing the argument that may or may not
#' be a nested matrix function  If this arg is not a simple reference to a 
#' global Rvar, the function proceeds to generate code to execute the nested
#' matrix function and save the result in an intermediate evaluation Rvar.
#' @param var_names A character vector that represents the the named R variables
#' included in these commands.
#' 
#' @returns Character vector representing the lines of code needed to evaluate
#' the nested matrix function and save the results in the newly created 
#' intermediate Rvar that will then be referenced by the parent expression.
#' @examples
#' get_intermediate_evaluation(arg, var_names)
get_intermediate_evaluation <- function(
    arg, 
    var_names
) {

  # If argument is a global variable, we do not need to evaluate it in an 
  # intermediate step, the data will be directly read from a global Rvar
  if (arg %in% var_names | arg %in% g_int_eval_env$expr_to_eval_map) return(NULL)
  
  # General case where the argument is itself an expression to be evaluated,
  # thus we will create an intermediate evaluation Rvar structure and 
  # evalute the expression, save the values in that Rvar structure, then 
  # use a reference to the intermediate evaluation Rvar structure as the 
  # returned value to be passed to the parent expression
  
  # Parse the expression to get the compiled lines of code necessary to
  # evaluate this intermediate argument, note that it is possible that this 
  # recursive call will itself have intermediate evaluations
  parsed_expr_arg_lines <- parse_expr(arg, var_names)
  
  # In the general case, save dimensional info of the expression as an 
  # intermediate evaluation, this creates a new Rvar structure in the 
  # intermediate evaluation global array in compiled code
  save_dim_info(arg, g_int_eval_env)
  
  # If there are additional levels of nested intermediate evaluations, 
  # capture and separate them here
  additional_lines <- get_additional_lines(list(parsed_expr_arg_lines))
  
  # The expression that evaluates the current intermediate evaluation
  current_int_expr <- parsed_expr_arg_lines[length(parsed_expr_arg_lines)]
  
  # Save the expressions dimensional info immediately before creating assign loop,
  # since assign loop functionality relies on global variables that are updated
  # by this process
  save_dim_info(arg, g_expr_env)
  
  # Create compiled code to assign the intermediate expression's value to 
  # the newly created intermediate expression Rvar structure
  final_expr_assign_lines <- write_assign_loop(g_int_eval_env$count,
                                               current_int_expr,
                                               var_mapping = GPU_INTERMEDIATE_EVAL_MAPPING)

  # Return any further nested intermediate evaluations, then currently
  # evaluated intermediate evaluation, and then sync the grid to ensure that
  # the parent expression does not begin evaluation until this intermediate
  # step is complete
  return(c(additional_lines, final_expr_assign_lines, SYNC_GRID))
}