

parse_matrix_expr <- function(expr_chars, raw_fun_str, var_names, var_mapping,
                              allocate_intermediate_exprs, parsed_fun_str) {
  
  # If the expression is being parsed to identify lengths or dimensions
  # and not to write the kernel, return the intermediate evaluation
  # Rvar structure with memory access available on either GPU or CPU
  # dependent on the var_mapping

  if (expr_chars %in% g_int_eval_env$expr_to_eval_map &
      !allocate_intermediate_exprs) {
    var_index <- which(g_int_eval_env$expr_to_eval_map == expr_chars)
    return(get_ref(var_index, var_mapping = var_mapping))
  }
  
  # Identify the matrix argument references and any additional lines of code
  # needed for intermediate evaluation
  parsed_info <- get_matrix_arg_refs(expr_chars, raw_fun_str, var_names,
                                     allocate_intermediate_exprs = allocate_intermediate_exprs)
  additional_lines <- parsed_info$additional_lines
  parsed_args <- parsed_info$parsed_args

  compiled_args <- paste(c(parsed_args, EVAL_DATA_INDEX), collapse = ", ")
  cur_expr <- paste0(parsed_fun_str, "(", compiled_args, ")")
  
  return(c(additional_lines, cur_expr))
}