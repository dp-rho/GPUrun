
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

  # The index offset expression will be returned, this expression
  # will evaluate to the correct offset given the parsed arguments
  # and dimensions based on the evaluation data index in each 
  # relevant thread
  index_offset_expr <- NULL_ARG
  
  # In base R, using a single matrix argument can be interpreted as using
  # coordinates with 2 row matrices, this feature is not implemented in GPUrun,
  # matrix arguments in the index function are always treated as vectors
  
  # Note: In all cases, evaluated expression must have 1 subtracted to offset
  # for the difference between C and R indexing (1:n vs 0:(n-1))
  
  # Case with only a single argument, i.e. x[2:10], 
  # each individual element (2 through 10) is offset from first element
  if (length(parsed_args) == 1) {
    index_offset_expr <- paste("(int)", parsed_args[1], "-", "1")
  }
  
  # Two dimensional case, offset dependent on dimensions

  # number of rows for variable being assigned to
  var_rows <- paste0(var_struct, ".", RDIM_TYPE)
  
  # Empty first arg, i.e., X[,3:4], 
  # offset to 3rd through 4th column
  if (parsed_args[1] == NULL_ARG) {
    
    # Index of evaluation in the column specifying arguments,
    # must vary between 1 and 2 in our example of X[,3:4]
    col_index <- paste0("(", EVAL_DATA_INDEX, " / ", var_rows, ")")
    
    # Column evaluation, the value of the expression determining
    # which columns are selected given this eval data index,
    # would vary between 3 and 4 in our example of X[,3:4]
    col_eval <- gsub(EVAL_DATA_INDEX, col_index, parsed_args[2])

    # Index offset based on the column alone
    col_offset <- paste0("(", col_eval, " * ", var_rows, ")")
    
    # Row offset given a specified column
    row_offset <- paste0("(", EVAL_DATA_INDEX, " % ", var_rows, ")")

    # The expression to take the column offset and row offset and calculate the index
    index_offset_expr <- paste("(int)", col_offset, "+", row_offset, "-", "1")
  }

  # Empty second arg, i.e., X[3:4,], 
  # offset to 3rd through 4th row
  if (parsed_args[2] == NULL_ARG) {
    
    # Index of evaluation in the column specifying arguments,
    # must vary between 1 and ncol(X) in our example of X[3:4,]
    col_index <- paste0("(", EVAL_DATA_INDEX, " / ", parsed_dims[1]$len, ")")

    # Index offset based on the column alone
    col_offset <- paste0("(", col_index, " * ", var_rows, ")")

    # Row index, would vary between 1 and 2 in the example of X[3:4,]
    row_index <- paste0("(", EVAL_DATA_INDEX, " % ", parsed_dims[1]$len, ")")

    # Row evaluation, the value of the expression determining which rows
    # are selected given this eval data index, 
    # would vary between 3 and 4 in the example of X[3:4,]
    row_eval <- gsub(EVAL_DATA_INDEX, row_index, parsed_args[1])

    # The expression to take the column and row offsets and calculate the index
    index_offset_expr <- paste("(int)", col_offset, "+", row_eval, "-", "1")
  }

  # General case, i.e., X[3:4,5:7]
  # Offset to 3rd through 4th row for 5th to 7th columns
  else {

    # Index of evaluation in the column specifying arguments,
    # must vary between 1 and 3 in our example of X[3:4,5:7]
    col_index <- paste0("(", EVAL_DATA_INDEX, " / ", parsed_dims[1]$len, ")")

    # Column evaluation, the value of the expression determining
    # which columns are selected given this eval data index,
    # would vary between 3 and 4 in our example of X[,3:4]
    col_eval <- gsub(EVAL_DATA_INDEX, col_index, parsed_args[2])

    # Index offset based on the column alone
    col_offset <- paste0("(", col_eval, " * ", var_rows, ")")

    # Row index, would vary between 1 and 2 in the example of X[3:4,5:7]
    row_index <- paste0("(", EVAL_DATA_INDEX, " % ", parsed_dims[1]$len, ")")

    # Row evaluation, the value of the expression determining which rows
    # are selected given this eval data index, 
    # would vary between 3 and 4 in the example of X[3:4,5:7]
    row_eval <- gsub(EVAL_DATA_INDEX, row_index, parsed_args[1])

    # The expression to take the column and row offsets and calculate the index
    index_offset_expr <- paste("(int)", col_offset, "+", row_eval, "-", "1")
  }

  return(index_offset_expr)
}

