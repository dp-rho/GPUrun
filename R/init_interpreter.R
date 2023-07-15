g_loop_env <- new.env(parent = .GlobalEnv)
g_expr_env <- new.env(parent = .GlobalEnv)
g_int_eval_env <- new.env(parent = .GlobalEnv)

# Initializes all of the globally tracked variables used for parsing and 
# storing machine generated expressions that determine iteration loop lengths
init_iter_loop <- function(var_names) {
  assign("g_loop_exprs", rep("0", MAX_LOOPS), envir = g_loop_env)
  assign("cur_expr", "", envir = g_loop_env)
  assign("parse_expr_len", parse_expr_len, envir = g_loop_env)
  assign("g_loop_count", 0, envir = g_loop_env)
  assign("var_names", var_names, envir = g_loop_env)
}

# Initializes all of the globally tracked variables used for parsing and 
# storing machine generated expressions that determine expression lengths
init_expr_lens <- function(var_names) {
  assign("g_expr_lens", rep("0", MAX_EXPRS), envir = g_expr_env)
  assign("cur_expr", "", envir = g_expr_env)
  assign("parse_expr_len", parse_expr_len, envir = g_expr_env)
  assign("g_expr_count", 0, envir = g_expr_env)
  assign("var_names", var_names, envir = g_expr_env)
}

# Initializes all of the globally tracked variables used for parsing and 
# storing machine generated expressions that determine intermediate evaluations
init_int_evals <- function(var_names) {
  assign("g_int_eval_lens", rep("0", MAX_INT_EVAL), envir = g_int_eval_env)
  assign("g_int_eval_rdims", rep("0", MAX_INT_EVAL), envir = g_int_eval_env)
  assign("g_int_eval_cdims", rep("0", MAX_INT_EVAL), envir = g_int_eval_env)
  assign("expr_to_eval_map", list(), envir = g_int_eval_env)
  assign("cur_expr", "", envir = g_int_eval_env)
  assign("parse_expr_len", parse_expr_len, envir = g_int_eval_env)
  assign("g_int_eval_count", 0, envir = g_int_eval_env)
  assign("var_names", var_names, envir = g_int_eval_env)
}