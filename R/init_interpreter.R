# Define maximum number of loops, assignment expressions, and intermediate
# evaluations possible in the code. Note that these are also defined as 
# global constants in the compiled code.
MAX_LOOPS <- 10
MAX_EXPRS <- 50
MAX_INT_EVAL <- 50


# Define terminology for dimensions in compiled code
LEN_TYPE <- "len"
RDIM_TYPE <- "rdim"
CDIM_TYPE <- "cdim"
FIELD_OF <- "."
DIMS <- c(LEN_TYPE, RDIM_TYPE, CDIM_TYPE)


# Create environments for globally tracking interpreter variables used 
# in parsing dimensional information 
g_loop_env <- new.env(parent = .GlobalEnv)
g_expr_env <- new.env(parent = .GlobalEnv)
g_int_eval_env <- new.env(parent = .GlobalEnv)
g_linalg_env <- new.env(parent = .GlobalEnv)

# Currently g_linalg_env is used only to track the largest dimension
# needed for a linalg function, as linalg functions require additional
# background memory
assign("linalg_dims", c(), envir = g_linalg_env)


# The functions used to write the stored dimensional information into the  
# initialize functions in compiled code
WRITE_FUNS <- list(
  
  # For loop case
  'loop' =  function(
    index_to_write,
    expr_to_write
  ) {
    return(paste0("g_iter_lens[", index_to_write - 1, "] = ", expr_to_write, ";"))
  },
  
  # Assignment expression case
  'assign' = function(
    index_to_write,
    expr_to_write
  ) {
    save_len <- paste0("expr_len = ", expr_to_write, ";")
    update_evals <- paste0("g_evals_per_thread[", index_to_write - 1, "] = ",
                           "ceil((float) expr_len / grid_size);")
    return(c(save_len, update_evals))
  },
  
  # Intermediate evaluation Rvar case
  'int_eval' = function(
    index_to_write,
    expr_to_write
  ) {
    len_assign <- paste0("len = ", expr_to_write$len, ";")
    var_assign <- paste0("g_int_evals[", index_to_write - 1, "] = {")
    var_inits <- c(".data = (double*) malloc_device(sizeof(double) * len),",
                   ".len = len,",
                   paste0(".rdim = (int) ", expr_to_write$rdim, ","),
                   paste0(".cdim = (int) ", expr_to_write$cdim))
    var_inits <- c(indent_lines(var_inits), "};")
    return(c(len_assign, var_assign, var_inits))
  }
)


# The R expressions used to update the stored dimensional information
# in the globally accessible R environments
UPDATE_EXPRS <- list(
  
  # For loop case
  'loop' = substitute(exprs_to_write[count + 1] <- parse_expr_dim(cur_expr, var_names)$len),
  
  # Assignment expression case
  'assign' = substitute(exprs_to_write[count + 1] <- parse_expr_dim(cur_expr, var_names)$len),
  
  # Intermediate evaluation Rvar case
  'int_eval' = substitute({
    exprs_to_write[[count + 1]] <- parse_expr_dim(cur_expr, var_names, 
                                                  type='return_size')
    expr_to_eval_map <- append(expr_to_eval_map, cur_expr)
  })
)


# The initial empty storage structures used to store dimensional information
INIT_STORAGE <- list(
  
  # For loop case
  'loop' = rep("EMPTY", MAX_LOOPS),
  
  # Assignment expression case
  'assign' = rep("EMPTY", MAX_EXPRS),
  
  # Intermediate evaluation Rvar case
  'int_eval' = replicate(MAX_INT_EVAL, 
                               setNames(vector("list", length(DIMS)), DIMS), 
                               simplify = FALSE)
)


# The compiled code variable names for specified dimensional information count
COUNT_VARS <- list(
  
  # For loop case
  'loop' = "g_iter_count",
  
  # Assignment expression case
  'assign' = "g_expr_count",
  
  # Intermediate evaluation Rvar case
  'int_eval' = "g_int_eval_count"
)


# The compiled code flag ids for specified dimensional information
FLAG_STRS <- list(
  
  # For loop case
  'loop' = "Iter.lens",
  
  # Assignment expression case
  'assign' = "Expr.lens",
  
  # Intermediate evaluation Rvar case
  'int_eval' = "Int.evals"
)


# The environments themselves matched by environment key
ENVS <- list(
  
  # For loop case
  'loop' = g_loop_env,
  
  # Assignment expression case
  'assign' = g_expr_env,
  
  # Intermediate evaluation Rvar case
  'int_eval' = g_int_eval_env
)


#' @title Initialize interpreter variables for specified dimensional information
#' 
#' @description
#' Initializes globally tracked variables used for parsing dimensional 
#' information in the compiled commands.The variables initialized will be
#' used to write the dimensional information to the initialize_iter_lens(),
#' initialize_expr_lens, or initialize_int_evals() functions in kernel.cu 
#' depending on which environment is specified.
#' 
#' @param var_names A character vector that represents the the named R variables
#' included in these commands.
#' @param env_key A character string identifying the environment tracked 
#' globally by the interpreter that stores dimensional information for iteration 
#' loops, assignment expressions, or intermediate evaluation Rvars for nested 
#' matrix function calls.
#' 
#' @returns NULL
#' @examples
#' init_interpreter(var_names, env_to_init)
init_interpreter <- function(
    var_names,
    env_key = c('loop', 'assign', 'int_eval')
) {
  
  # Match args
  env_key <- match.arg(env_key)
  
  # Define the writing function for iteration loop expression parsing
  write_fun <- WRITE_FUNS[[env_key]]
  
  # Define the expression used to update the dimension information stored
  # in the loop environment
  update_expr <- UPDATE_EXPRS[[env_key]]
  
  # Initial empty storage structure
  init_exprs <- INIT_STORAGE[[env_key]]
  
  # Count variable name in for the specified dimensional information in
  count_str <- COUNT_VARS[[env_key]]
  
  # Flag id for identifying where in kernel.cu to write information to
  flag_str <- FLAG_STRS[[env_key]]
  
  # Retrieve the environment itself
  env_to_init <- ENVS[[env_key]]
  
  # Assign values to all globally tracked variables in the environment
  assign("write_fun", write_fun, envir = env_to_init)
  assign("update_expr", update_expr, envir = env_to_init)
  assign("exprs_to_write", init_exprs, envir = env_to_init)
  assign("count_str", count_str, envir = env_to_init)
  assign("flag_str", flag_str, envir = env_to_init)
  assign("var_names", var_names, envir = env_to_init)
  assign("cur_expr", "", envir = env_to_init)
  assign("parse_expr_dim", parse_expr_dim,, envir = env_to_init)
  assign("count", 0, envir = env_to_init)
  assign("expr_to_eval_map", list(), envir = g_int_eval_env)
  
  return(NULL)
}

