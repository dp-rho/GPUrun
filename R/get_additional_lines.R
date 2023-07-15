# Takes arguments representing the lines of machine generated code
# that may or may not have additional lines for each argument.
# Iterate through the arguments and return a single vector of all
# intermediate evaluation lines that must be executed prior to 
# the final expression that uses these arguments
get_additional_lines <- function(args) {
  additional_lines <- c()
  for (arg in args) {
    if (length(arg) > 1) {
      additional_lines <- c(additional_lines, arg[1:(length(arg) - 1)])
    }
  }
  return(additional_lines)
}