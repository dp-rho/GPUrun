#' @title Retrieve intermediate evaluation lines from parsed arguments
#' 
#' @description
#' Takes arguments representing the lines of machine generated code
#' that may or may not have additional lines for each argument.
#' Iterate through the arguments and return a single vector of all
#' intermediate evaluation lines that must be executed prior to 
#' the final expression that uses these arguments
#' 
#' @param parsed_args A list of character vectors that represent parsed lines
#' of code which include the current arguments and any intermediate evaluation
#' lines needed before those arguments are used.
#' 
#' @returns A character vector of lines of code for all intermediate evaluations
#' needed before the current arguments are used.
#' @examples
#' get_additional_lines(parsed_args)
get_additional_lines <- function(parsed_args) {
  
  # Define function to lapply over the input parsed_args, remove the last line
  trim_last_line <- function(lines) {
    return(lines[1:(length(lines) - 1)])
  }
  
  # Apply trimming function over all arguments
  trimmed_args <- lapply(parsed_args, 
                         function(arg) {if (length(arg) > 1) trim_last_line(arg)})

  # Return the lines in a single vector
  return(unlist(trimmed_args))
}