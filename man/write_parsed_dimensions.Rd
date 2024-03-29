% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_parsed_dimensions.R
\name{write_parsed_dimensions}
\alias{write_parsed_dimensions}
\title{Writes code to update a compiled initialization
of dimensions}
\usage{
write_parsed_dimensions(
  lines_to_edit,
  g_var_env = c(g_int_eval_env, g_loop_env, g_expr_env)
)
}
\arguments{
\item{lines_to_edit}{A character vector representing the lines of code in
kernel.cu that will be updated by this function call}

\item{g_var_env}{A globally accessible environment that stores all of the 
relevant parsed expressions and other information necessary to write the
machine generated code}
}
\value{
Character vector representing the updated lines to be written
to kernel.cu
}
\description{
Writes dimension information stored in a globally accessible environment
for either intermediate evaluations, expression lengths, or loop iterations
to the input vector of character strings that represents the lines of 
code that must be updated in kernel.cu
}
\examples{
write_parsed_dimensions(kernel_lines, g_loop_env)
}
