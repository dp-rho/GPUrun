# Writes machine generated lines to create an intermediate evaluation step for 
# the provided argument to some matrix function, additionally, global variables
# used to track intermediate evaluations are updated, and the written lines
# are returned
get_intermediate_evaluation <- function(
    arg, 
    var_names
) {
  
  # If argument is a global variable, we do not need to evaluate it in an 
  # intermediate step, the data will be directly read from a global Rvar
  if (arg %in% var_names) return(NULL)
  
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