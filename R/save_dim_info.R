#' @title Saves dimensional information of iteration loop
#' 
#' @description
#' Saves the necessary dimensional information relating to an expression. The 
#' expression's dimension may be related to updating values of global variables, 
#' controlling the number of iterations in a for loop, or an intermediate
#' evaluation of a nested matrix argument.
#' 
#' @param arg A character string representing the expression that must be parsed
#' for dimensional information.
#' @param env_to_update A environment that stores the necessary dimensional 
#' information for assignment expressions, for loops, or intermediate matrix
#' evaluations.
#' 
#' @returns NULL
#' @examples
#' save_dim_info(arg, env_to_update)
save_dim_info <- function(
    arg,
    env_to_update
) {
  
  # Save the provided argument in the specified environment
  env_to_update$cur_expr <- arg
  
  # Update the stored dimension information based on the saved expression
  eval(env_to_update$update_expr, envir = env_to_update)
  
  # Increment the global count of this type of dimensional information
  env_to_update$count <- env_to_update$count + 1
  
  return(NULL)
}

