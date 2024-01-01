parse_json_dim <- function(
    args,
    expr_chars,
    var_names,
    fun_data,
    dim_type = c('num_evals', 'return_size')
) {
  
  if ('parse_vals' %in% names(fun_data)) {
    eval(str2lang(fun_data[['parse_vals']]))
  }
  
  parsed_dims <- lapply(args, parse_expr_dim, var_names=var_names, 
                        dim_type=dim_type)
  
  len <- eval(str2lang(fun_data[['len']]))
  rdim <- eval(str2lang(fun_data[['rdim']]))
  cdim <- eval(str2lang(fun_data[['cdim']]))

  return(list(len=len, rdim=rdim, cdim=cdim))
}