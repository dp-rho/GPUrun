
parse_fun <- function(
    expr_chars,
    var_names,
    types = 'data',
    index = '_eval_data_index',
    var_mapping = c(GPU_MAPPING, CPU_MAPPING, GPU_INTERMEDIATE_EVAL_MAPPING,
                    CPU_INTERMEDIATE_EVAL_MAPPING),
    depth = 0,
    allocate_intermediate_exprs = TRUE,
    parser = c('evaluation', 'dimension'),
    dim_type = c('num_evals', 'return_size')
) {
  
  # Match args
  parser <- match.arg(parser)
  var_mapping <- match.arg(var_mapping)
  dim_type <- match.arg(dim_type)
  
  # Get correct json dict based on parser
  parser_dict <- g_fun_env$fun_dict
  if (parser == 'dimension') {
    parser_dict <- g_fun_env$dim_dict
  }
  
  # Index of the first white space " " separator
  first_sep_index <- unlist(gregexpr(' ', expr_chars))[1]
  
  # Isolate the function string by incrementing past opening '(' and then
  # capturing up to the first white space separator
  fun_str <- substr(expr_chars, 2, first_sep_index - 1)
  
  # Loop through each function entry to identify match
  for (fun_data in parser_dict) {

    # Check if function being parsed exists in the associated names
    if (fun_str %in% fun_data[['function_key']]) {

      args_start <- nchar(fun_str) + 3
      args <- identify_args(substr(expr_chars, args_start, nchar(expr_chars)))
      
      # Initialize lines of compiled code to be returned
      compiled_lines <- c()
      
      # If parsing for evaluation, call correct parser
      if (parser == 'evaluation') { 
        compiled_lines <- parse_json_eval(fun_str, args, expr_chars, var_names, 
                                          types, index, fun_data, var_mapping, 
                                          depth, allocate_intermediate_exprs)
      }
      
      # Else call parser for dimension
      else if (parser == 'dimension') {
        compiled_lines <- parse_json_dim(args, expr_chars, var_names, fun_data,
                                         dim_type=dim_type)
      }
      
      return(compiled_lines)
    }
  }
  cat("ERROR: unmatched function\n")
}