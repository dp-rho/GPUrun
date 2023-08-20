MAX_LOOPS <- 10
MAX_EXPRS <- 50
MAX_INT_EVAL <- 50

g_loop_env <- new.env(parent = .GlobalEnv)
g_expr_env <- new.env(parent = .GlobalEnv)
g_int_eval_env <- new.env(parent = .GlobalEnv)

# Initializes all of the globally tracked variables used for parsing and 
# storing machine generated expressions that determine iteration loop lengths
init_iter_loop <- function(
    var_names
) {
  
  # Define the writing function for iteration loop expression parsing
  write_fun <- function(
    index_to_write,
    expr_to_write
  ) {
    return(paste0("g_iter_lens[", index_to_write - 1, "] = ", expr_to_write, ";"))
  }
  
  update_expr <- substitute(exprs_to_write[count + 1] <- parse_expr_dim(cur_expr, var_names)$len)
  
  # Assign values to all globally tracked variables in the environment
  assign("exprs_to_write", rep("EMPTY", MAX_LOOPS), envir = g_loop_env)
  assign("cur_expr", "", envir = g_loop_env)
  assign("parse_expr_dim", parse_expr_dim,, envir = g_loop_env)
  assign("count", 0, envir = g_loop_env)
  assign("count_str", "g_iter_count", envir = g_loop_env)
  assign("var_names", var_names, envir = g_loop_env)
  assign("write_fun", write_fun, envir = g_loop_env)
  assign("flag_str", "Iter.lens", envir = g_loop_env)
  assign("update_expr", update_expr, envir = g_loop_env)
}

# Initializes all of the globally tracked variables used for parsing and 
# storing machine generated expressions that determine expression lengths
init_expr_lens <- function(
    var_names
) {
  
  # Define the writing function for expression length and evaluations per 
  # thread for each expression in the kernel call
  write_fun <- function(
    index_to_write,
    expr_to_write
  ) {
    save_len <- paste0("expr_len = ", expr_to_write, ";")
    update_evals <- paste0("g_evals_per_thread[", index_to_write - 1, "] = ",
                           "std::ceil((float) expr_len / grid_size);")
    return(c(save_len, update_evals))
  }
  
  update_expr <- substitute(exprs_to_write[count + 1] <- parse_expr_dim(cur_expr, var_names)$len)
  
  assign("exprs_to_write", rep("EMPTY", MAX_EXPRS), envir = g_expr_env)
  assign("cur_expr", "", envir = g_expr_env)
  assign("parse_expr_dim", parse_expr_dim, envir = g_expr_env)
  assign("count", 0, envir = g_expr_env)
  assign("count_str", "g_expr_count", envir = g_expr_env)
  assign("var_names", var_names, envir = g_expr_env)
  assign("write_fun", write_fun, envir = g_expr_env)
  assign("flag_str", "Expr.lens", envir = g_expr_env)
  assign("update_expr", update_expr, envir = g_expr_env)
}

# Initializes all of the globally tracked variables used for parsing and 
# storing machine generated expressions that determine intermediate evaluations
init_int_evals <- function(var_names) {
  
  # Define the writing function for initializing the intermediate evaluation
  # Rvar structures used in the kernel 
  write_fun <- function(
    index_to_write,
    expr_to_write
  ) {
      len_assign <- paste0("len = ", expr_to_write$len, ";")
      var_assign <- paste0("g_int_evals[", index_to_write - 1, "] = {")
      var_inits <- c(".data = (double*) malloc_device(sizeof(double) * len),",
                     ".len = len,",
                     paste0(".rdim = ", expr_to_write$rdim, ","),
                     paste0(".cdim = ", expr_to_write$cdim))
      var_inits <- c(indent_lines(var_inits), "};")
    return(c(len_assign, var_assign, var_inits))
  }
  
  update_expr <- substitute({
    exprs_to_write[[count + 1]] <- parse_expr_dim(cur_expr, var_names)
    expr_to_eval_map <- append(expr_to_eval_map, cur_expr)
  })

  dim_names <- c("len", "rdim", "cdim")
  empty_dims_list <- replicate(MAX_INT_EVAL, 
                               setNames(vector("list", length(dim_names)), dim_names), 
                               simplify = FALSE)
  assign("exprs_to_write", empty_dims_list, envir = g_int_eval_env)
  assign("expr_to_eval_map", list(), envir = g_int_eval_env)
  assign("cur_expr", "", envir = g_int_eval_env)
  assign("parse_expr_dim", parse_expr_dim,, envir = g_int_eval_env)
  assign("count", 0, envir = g_int_eval_env)
  assign("count_str", "g_int_eval_count", envir = g_int_eval_env)
  assign("var_names", var_names, envir = g_int_eval_env)
  assign("write_fun", write_fun, envir = g_int_eval_env)
  assign("flag_str", "Int.evals", envir = g_int_eval_env)
  assign("update_expr", update_expr, envir = g_int_eval_env)
}