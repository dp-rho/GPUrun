NULL_ARG <- ''

#' @title Identify all arguments of a racket like R expression
#' 
#' @description
#' Repeatedly iterate through a string representing a racket like R expression,
#' specifically, an expression with the general form (f ...) with any number of 
#' arguments and identify the all arguments of the expression.
#' 
#' @param expr_chars A character string representing a function call R 
#' expression in racket like format for which the function string itself
#' has already been removed from consideration, leaving only the arguments
#' to be identified.
#' 
#' @returns A character vector representing the distinct arguments of 
#' the current expression.
#' @examples
#' identify_args(expr_chars)
identify_args <- function(expr_chars) {
  cur_pos <- 1
  args <- c()
  while (cur_pos <= nchar(expr_chars)) {
    arg_len <- identify_arg(substr(expr_chars, cur_pos, nchar(expr_chars)))
    if (arg_len == IS_NULL_ARG) {
      arg_chars <- NULL_ARG
    }
    else {
      arg_chars <- substr(expr_chars, cur_pos, cur_pos + arg_len - 1)
    }
    args <- c(args, arg_chars)
    cur_pos <- cur_pos + arg_len + 1
    if (arg_len == IS_NULL_ARG) break
  }
  return(args)
}