% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/identify_arg.R
\name{identify_arg}
\alias{identify_arg}
\title{Identify the next argument in a racket like R expression}
\usage{
identify_arg(expr_chars)
}
\arguments{
\item{expr_chars}{A character string representing whatever remains of the
expression for which arguments are being identified.}
}
\value{
An integer representing the character index of the end of the next
argument identified, using the first character of the argument as index 0.
If no argument was identified, 0 will be returned.
}
\description{
Iterate through a string representing a racket like R expression,
specifically, an expression with the general form (f ...) with any number of 
arguments and identify the next argument.  Note it possible that one or more
arguments have already been identified and this function is called when
there are no additional arguments left to identify.
}
\examples{
identify_arg(expr_chars)
}
