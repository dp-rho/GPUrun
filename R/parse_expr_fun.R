

parse_fun_dim <- function(
    expr_chars, 
    var_names,
    type = c('num_evals', 'return_size')
) {
  
  # Index of the first white space " " separator
  first_sep_index <- unlist(gregexpr(' ', expr_chars))[1]
  
  # Isolate the function string by incrementing past opening '(' and then
  # capturing up to the first white space separator
  fun_str <- substr(expr_chars, 2, first_sep_index - 1)
  
  # Loop through each function entry to identify match
  for (fun_data in g_fun_env$fun_dict) {
    
    # Check if function being parsed exists in the associated names
    if (fun_str %in% fun_data[['function_key']]) {
      
      
    }
  }
}