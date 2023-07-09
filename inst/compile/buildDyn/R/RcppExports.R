# Machine updated code for calling linked .so library
CCxAAAA_execute_commands <- function() {
  invisible(.Call(`_CCxAAAA_execute_commands`))
}

CCxAAAA_bind_vars <- function(vars, eval_env) {
  for (numeric_var_str in vars) {
    numeric_var <- eval(parse(text=numeric_var_str), envir=eval_env)
    dimension <- dim(numeric_var)
    if (is.null(dimension)) {
      dimension <- c(0, 0)
    }
    invisible(.Call(`_CCxAAAA_bind_var`, numeric_var, dimension))
  }
}

CCxAAAA_update_vars <- function(vars, eval_env) {
  for (i in seq_along(vars)) {
    data_vec <- invisible(.Call(`_CCxAAAA_get_data`, i))
    extracted_dim <- dim(eval(parse(text = vars[i]), envir = eval_env))
    if (!is.null(extracted_dim)) {
      assign(vars[i], matrix(data_vec,
                             nrow=extracted_dim[1],
                             ncol=extracted_dim[2]),
             eval_env)
    }
    else {
      assign(vars[i], data_vec, eval_env)
    }
  }
}
