INDENT <- "  "

#' @title Indent input lines
#' 
#' @description
#' Use a pre specified indentation to indent the lines input by the specified
#' depth of indentation.
#' 
#' @param lines A character vector representing the lines of text to indent
#' @param depth An integer representing the number of indentations to apply
#' 
#' @returns A character vector representing the input lines with the requested
#' indentation applied
#' @examples
#' indent_lines(lines, depth)
indent_lines <- function(lines, depth = 1) {
  indent <- paste(rep(INDENT, depth), collapse = "")
  return(paste0(indent, lines))
}