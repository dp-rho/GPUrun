
##
## Returns expression for index offset given a data index.
## This expression can be attached to an expression either when
## memory is being assigned to or when memory is being read from,
## as indexing can occur for assignment or read access
parse_index_expr <- function(
  parsed_args, 
  parsed_dims, 
  var_struct
) {

  # In base R, using a single matrix argument can be interpreted as using
  # coordinates with 2 row matrices, this feature is not implemented in GPUrun,
  # matrix arguments in the index function are always treated as vectors
  
  # Note: In all cases, evaluated expression must have 1 subtracted to offset
  # for the difference between C and R indexing (1:n vs 0:(n-1))
  
  # Case with only a single argument, i.e. x[1:10], 
  # each element is offset from 0
  if (length(parsed_args) == 1) {
    return(paste("(int)", parsed_args[1], "-", "1"))
  }
  
  # Two dimensional case, offset dependent on dimensions
  
  # Empty first arg, i.e., X[,3:4], 
  # offset to 3rd through 4th column
  if (parsed_args[1] == NULL_ARG) {
    
    # number of rows for variable being assigned to
    var_rows <- paste0(var_struct, ".", RDIM_TYPE)
    
    # Column index 
    col_index <- paste0("(", EVAL_DATA_INDEX, " / ", var_rows, ")")
    
    # Column offset, instead of evaluating the vector 3:4 at index
    # EVAL_DATA_INDEX (the default), we evaluate it at the specified
    # col index calculated above
    col_offset <- gsub(EVAL_DATA_INDEX, col_index, parsed_args[2])
    
    # Row offset given a specified column
    row_offset <- paste0("(", EVAL_DATA_INDEX, " % ", var_rows, ")")
    
    return(paste("(int)", col_offset, "+", row_offset, "-", "1"))
  }
  
}

