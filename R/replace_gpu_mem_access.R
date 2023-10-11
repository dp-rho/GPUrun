
# Identifies and replaces references to gpu memory, i.e., g_vars.data[x]
# with local cpu memory, access_mem[i], with i starting at 0 and 
# growing to the maximum number of gpu memory references.  The gpu
# memory is also copied into the host memory using memcpy_to_host
replace_gpu_mem_access <- function(
    compiled_code_lines,
    expr_flag_str = c("Iter.lens","Expr.lens", "Int.evals"),
    mem_flag_str = c("Iter.mem", "Expr.mem", "Int.mem")
) {
  
  # Identify the lines of code where memory accesses must be replaced
  line_indices <- find_start_end_lines(compiled_code_lines, expr_flag_str)
  
  # Initialize the tracker of memory accesses
  access_env <- environment()
  access_env$ref_count <- 1
  access_env$mem_accesses <- c()
  
  # Need to recursively replaced any access to {g_vars/g_int_evals}.data[{any expr}]
  # with local CPU memory access, called access_mem[i], with i starting at 0 
  # and increasing to the total number of unique memory access expressions
  
  # Define a local recursive function
  parse_access_info <- function(expr_chars) {
    
    # The string pattern that determines if a memory access exists
    mem_access_pattern <- "([a-z_]+?)\\[(\\d+)\\].data\\[(.*)"
    
    # If there is no nested memory access expressions, read up to first "]"
    if (!(grepl(mem_access_pattern, expr_chars))) {
      return(expr_chars)
    }
    else {
      # browser()
      match_result <- stringr::str_match(expr_chars, mem_access_pattern)
      mapping <- match_result[1, 2]
      var_index <- match_result[1, 3]
      remaining_chars <- match_result[1, 4]
      last_char <- index_expr_end(remaining_chars)
      index_expr <- substr(remaining_chars, 1, last_char)
      
      parsed_index_expr <- parse_access_info(index_expr)
      replaced_expr_chars <- gsub(index_expr, parsed_index_expr, match_result[1, 1], fixed = TRUE)
      
      old_ref <- paste0(mapping, "[", var_index, "].data[", parsed_index_expr, "]")
      new_ref <- paste0("access_mem[", access_env$ref_count - 1, "]")
      access_env$mem_accesses <-append(access_env$mem_accesses, list(list(mapping=mapping, var_index=var_index,
                                              parsed_index_expr=parsed_index_expr)))
      replaced_expr <- gsub(old_ref, new_ref, replaced_expr_chars, fixed=TRUE)
      access_env$ref_count <- access_env$ref_count + 1
      updated_expr_chars <- gsub(match_result[1, 1], replaced_expr, expr_chars, fixed=TRUE)
      return(updated_expr_chars)
    }
  }
  
  # Simple function for identifying when the index expression is over
  index_expr_end <- function(expr_chars) {
    open_count <- 0
    for (index in 1:nchar(expr_chars)) {
      char_at <- substr(expr_chars, index, index)
      if (char_at == '[') open_count <- open_count + 1
      else if (char_at == ']') {
        if (open_count == 0) return(index - 1)
        else open_count <- open_count - 1
      }
    }
    return(index)
  }
  
  # Replace the dimension expression lines with updated memory accesses
  replaced_lines <- c()
  for (line in compiled_code_lines[line_indices$start:line_indices$end]) {
    replaced_lines <- append(replaced_lines, parse_access_info(line))
  }
  compiled_code_lines[line_indices$start:line_indices$end] <- replaced_lines
  
  # initialize the new memory accesses
  copy_mem_lines <- find_start_end_lines(compiled_code_lines, mem_flag_str)
  init_lines <- c()
  for (index in 1:length(access_env$mem_accesses)) {
    mem_info <- access_env$mem_accesses[[index]]
    host_mem <- paste("access_mem", "+", index - 1)
    device_mem <- paste0(mem_info$mapping, "[", mem_info$var_index, "].data + ", mem_info$parsed_index_expr)
    mem_init_line <- paste0("memcpy_to_host(", host_mem, ", ", device_mem, ", sizeof(double))")
    init_lines <- append(init_lines, mem_init_line)
  }
  
  inserted_lines <- c(compiled_code_lines[1:copy_mem_lines$start],
                      indent_lines(paste0(init_lines, ";")),
                      compiled_code_lines[copy_mem_lines$end:length(compiled_code_lines)])
  return(inserted_lines)
}