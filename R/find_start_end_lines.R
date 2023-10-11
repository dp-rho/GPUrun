# Global constants used for identifying the start and end line indices 
# needed when updating machine generated code
FLAG_FORMATS <- list(start = "// [[{TYPE}::start]]",
                     end = "// [[{TYPE}::end]]")
SUB_TYPE_STR <- "{TYPE}"
START_FLAG <- -1
END_FLAG <- 1
OTHER_FLAG <- 0

#' @title Identify the region of text to replace in lines of compiled code
#' 
#' @description
#' Identifies the start and end line indices of a specified character string
#' that represents the type of machine generated code that will be updated
#' 
#' @param text_lines A character vector that represents the lines of compiled
#' code that will be used to search for the start and end indices of the
#' provided type
#' @param flag_str A character string representing the type of start and end
#' flags that are to be constructed.  Must be one of "Kernel", "Iter.lens",
#' "Expr.lens", or "Int.evals".
#' 
#' @returns Vector of start and end indices for the specified flag_str
#' @examples
#' find_start_end_lines("Kernel")
find_start_end_lines <- function(
    text_lines, 
    flag_str = c("Kernel", "Iter.lens","Expr.lens", "Int.evals",
                 "Iter.mem", "Expr.mem", "Int.mem")
) {
  
  # Match args
  flag_str <- match.arg(flag_str)
  
  # Construct and identify full flags in compiled code
  flag_strs <- construct_flags(flag_str)
  mapped_matches <- as.vector(lapply(text_lines, match_flags,
                                     start_flag_str = flag_strs$start,
                                     end_flag_str = flag_strs$end))
  start_index <- which(mapped_matches == START_FLAG)
  end_index <- which(mapped_matches == END_FLAG)
  return(list(start = start_index, end = end_index))
}

#' @title Matches compiled code flags against an input
#' 
#' @description
#' Match the input start and end flags against the input character
#' vector which is expected to be a line of code in kernel.cu. Return integer
#' that represents either the start index, end index, or other, which is ignored.
#' 
#' @param input_str A character string representing the line of text that 
#' will be searched for a match of either the start or end flag
#' @param start_flag_str A character string representing the start flag 
#' @param end_flag_str A character string representing the end flag
#' 
#' @returns Integer representing a flag that indicates whether to start, end,
#' or not relevant.
#' @examples
#' match_flags("Kernel")
match_flags <- function(
    input_str, 
    start_flag_str, 
    end_flag_str
) {
  if (grepl(start_flag_str, input_str, fixed = TRUE)) {
    return(START_FLAG)
  }
  if (grepl(end_flag_str, input_str, fixed = TRUE)) {
    return(END_FLAG)
  }
  return(OTHER_FLAG);
}

#' @title Create the character flags to search for in the compiled code
#' 
#' @description
#' Construct start and end flag strings to be matched against lines 
#' in machine updated kernel.cu file
#' 
#' @param flag_str A character string representing the type of start and end
#' flags that are to be constructed.  Must be one of "Kernel", "Iter.lens",
#' "Expr.lens", or "Int.evals".
#' 
#' @returns List of two character strings, a start flag and end flag.
#' @examples
#' construct_flags("Kernel")
construct_flags <- function(
    flag_str = c("Kernel", "Iter.lens", "Expr.lens", "Int.evals")
) {
  return(lapply(FLAG_FORMATS, gsub, pattern = SUB_TYPE_STR,
                replacement = flag_str, fixed = TRUE))
}
