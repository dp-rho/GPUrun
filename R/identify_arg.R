OPEN_EXPR <- "("
CLOSE_EXPR <- ")"
DELIM <- " "

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
        return(index - 1)
      }
      open_count <- open_count - 1
    }
  }
}