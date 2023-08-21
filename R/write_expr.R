#' @title Parse and interpret an R expression into CUDA code
#' 
#' @description
#' High level function used to parse and interpret a single R expression and
#' then generate CUDA code to execute that R expression on the GPU
#' 
#' @param expr An R expression to be parsed and interpreted into CUDA code
#' @param var_names A character vector that represents the the named R variables
#' included in these commands.
#' 
#' @returns A character vector representing the lines of code necessary
#' to execute the input expression in CUDA code
#' @examples
#' write_expr(expr, var_names)
write_expr <- function(expr, var_names) {
  
  # Translate the R expression into a string with the form (fun ...)
  racket_characters <- racket_string(expr)
  
  # Parse the string (fun ...) into lines of compiled code
  parsed_lines <- parse_expr(racket_characters, var_names)
  
  # Only add the ";" to the final line, as some expressions
  # will already be multi-line and not require a new ";"
  final_lines <- paste0(parsed_lines[length(parsed_lines)], ";")
  if (length(parsed_lines) > 1) {
    final_lines <- c(parsed_lines[1:(length(parsed_lines) - 1)],
                     final_lines)
  }
  
  return(final_lines)
}