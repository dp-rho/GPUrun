identify_args <- function(expr_chars) {
  cur_pos <- 1
  args <- c()
  while (cur_pos < nchar(expr_chars)) {
    arg_len <- identify_arg(substr(expr_chars, cur_pos, nchar(expr_chars)))
    arg_chars <- substr(expr_chars, cur_pos, cur_pos + arg_len - 1)
    args <- c(args, arg_chars)
    cur_pos <- cur_pos + arg_len + 1
  }
  return(args)
}