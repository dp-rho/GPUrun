#' @title Translate an R expression into a racket like string
#' 
#' @description
#' Recursive function which translates an R expression with up to any arbitrary 
#' level of nested arguments into a racket like string of the form '(fun ...)'.
#' 
#' @param input_expr An R expression that will be translated into a racket 
#' like character string.
#' 
#' @returns A character string that represents the translated expression in the
#' form of (fun ...) with any arbitrary level of nested arguments also 
#' translated to the same format.
#' @examples
#' racket_string(input_expr)
racket_string <- function(input_expr) {
  if (is.numeric(input_expr)) {
    return(deparse(input_expr))
  }
  else if (is.symbol(input_expr)) {
    return(as.character(input_expr))
  }
  else if (is.function(eval(input_expr[[1]]))) {
    function_string <- as.character(input_expr)
    
    # Create special replacement for the "(" function, as the open parentheses
    # is used as a open expression identifying character in our parsing
    if (substr(function_string[1], 1, 1) == "(") function_string <- 'par'
    
    # Add the open expression character before the function string,
    # then parse the arguments if needed
    expr_char_vec <- paste0("(", function_string[1])
    for (i in 2:length(input_expr)) {
      expr_char_vec <- paste0(expr_char_vec, " ",
                              racket_string(input_expr[[i]]))
    }
    
    # Add close expression character and return the string
    return(paste0(expr_char_vec, ")"))
  }
  print("ERROR: Expression unmatched ")
  print(as.character(input_expr))
  return(NULL)
}