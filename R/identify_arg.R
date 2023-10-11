OPEN_EXPR <- "("
CLOSE_EXPR <- ")"
DELIM <- " "
IS_NULL_ARG <- -1

#' @title Identify the next argument in a racket like R expression
#' 
#' @description
#' Iterate through a string representing a racket like R expression,
#' specifically, an expression with the general form (f ...) with any number of 
#' arguments and identify the next argument.  Note it possible that one or more
#' arguments have already been identified and this function is called when
#' there are no additional arguments left to identify.
#' 
#' @param expr_chars A character string representing whatever remains of the
#' expression for which arguments are being identified.
#' 
#' @returns An integer representing the character index of the end of the next
#' argument identified, using the first character of the argument as index 0.
#' If no argument was identified, 0 will be returned.
#' @examples
#' identify_arg(expr_chars)
identify_arg <- function(expr_chars) {
  open_count <- 0
  for (index in 1:nchar(expr_chars)) {
    char_at <- substr(expr_chars, index, index)
    if (char_at == DELIM & open_count == 0) {
      return(index - 1)
    }
    else if (char_at == OPEN_EXPR) {
      open_count <- open_count + 1
    }
    else if (char_at == CLOSE_EXPR) {
      if (open_count == 0) {
        if (index != 1) {
          return(index - 1)
        }
        else return(IS_NULL_ARG)
      }
      open_count <- open_count - 1
    }
  }
}