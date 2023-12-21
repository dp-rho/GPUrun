
parse_fun <- function(
    expr_chars,
    var_names,
    types,
    index,
    var_mapping = c(GPU_MAPPING, CPU_MAPPING, GPU_INTERMEDIATE_EVAL_MAPPING,
                    CPU_INTERMEDIATE_EVAL_MAPPING),
    depth = 0,
    allocate_intermediate_exprs=TRUE
) {
  
  # Function used to convert 'R__some_var' strings in json data
  # to actual R variables in parsing
  get_actual_var <- function(input_str) {
    if (startsWith(input_str, "R__")) {
      return(eval(str2lang(substr(input_str, 4, nchar(input_str)))))
    }
    return(input_str)
  }
  
  # Index of the first white space " " separator
  first_sep_index <- unlist(gregexpr(' ', expr_chars))[1]
  
  # Isolate the function string by incrementing past opening '(' and then
  # capturing up to the first white space separator
  fun_str <- substr(expr_chars, 2, first_sep_index - 1)
  
  # Loop through each function entry to identify match
  for (fun_data in g_fun_env$fun_dict) {

    # Check if function being parsed exists in the associated names
    if (fun_str %in% fun_data[['function_key']]) {

      args_start <- nchar(fun_str) + 3
      args <- identify_args(substr(expr_chars, args_start, nchar(expr_chars)))
      
      parsed_index <- which(fun_str == fun_data[['function_key']])
      
      # Fully custom R implementation special cases
      if ('custom_fun' %in% names(fun_data)) {
        return(eval(str2lang(fun_data[['custom_fun']][parsed_index])))
      }

      # Identify the indices of parsing for each argument, if no indices for 
      # argument parsing are specified, assume the input index is used
      if ('arg_indices' %in% names(fun_data)) {
        arg_indices <- unlist(lapply(fun_data[['arg_indices']], get_actual_var))
      }
      else {
        arg_indices <- index
      }

      # Identify the type of parsing for each argument
      if ('types' %in% names(fun_data)) {
        types <- fun_data[['types']]
      }
      
      # Parse args with identified information
      parsed_info <- parse_args(NULL, expr_chars,
                                var_names=var_names, indices=arg_indices, 
                                types=types, var_mapping=var_mapping,
                                input_args=args,
                                allocate_intermediate_exprs=allocate_intermediate_exprs)
      
      parsed_args <- parsed_info$cur_args
      additional_lines <- parsed_info$additional_lines
      
      # Execute the preprocess statements if needed
      if ('preprocess_call' %in% names(fun_data)) {
        for (caller in fun_data[['preprocess_call']])
          eval(str2lang(caller))
      }
      
      # Add additional arguments to function call if needed
      if ('compiled_args' %in% names(fun_data)) {
        compiled_args <- paste(unlist(lapply(fun_data[['compiled_args']], 
                                      get_actual_var)), collapse = ", ")
      }
      else {
        compiled_args <- paste(parsed_args, collapse=", ")
      }
      
      # If the function is has a __device__ implementation, create expression
      if ('parsed_func' %in% names(fun_data)) {
        cur_expr <- paste0(fun_data[['parsed_func']][parsed_index],
                           "(", compiled_args, ")")
      }
      
      # If the function has a custom expression implementation, use that
      else {
        cur_expr <- eval(str2lang(fun_data[['override_expr']]))
      }

      return(c(additional_lines, cur_expr))
    }
  }
  cat("ERROR: unmatched function")
}