# Writes machine generated lines to create an intermediate evaluation step for 
# the provided argument to some matrix function, additionally, global variables
# used to track intermediate evaluations are updated, and the written lines
# are returned
get_intermediate_evaluation <- function(arg, var_names) {
  
  parsed_expr_arg_lines <- parse_expr(arg, var_names, DEFAULT_DEPTH)
  save_int_eval(arg)
  additional_lines <- get_additional_lines(list(parsed_expr_arg_lines))
  final_expr <- parsed_expr_arg_lines[length(parsed_expr_arg_lines)]
  
  # Save the expressions meta info immediately before creating assign loop,
  # since assign loop functionality relies on global variables that are updated
  # by save_expr_len
  save_expr_len(arg)
  final_expr_assign_lines <- write_assign_loop(g_int_eval_env$g_int_eval_count,
                                               final_expr,
                                               var_mapping = GPU_INTERMEDIATE_EVAL_MAPPING)

  return(c(additional_lines, final_expr_assign_lines, SYNC_GRID))
}