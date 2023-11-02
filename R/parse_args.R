parse_args <- function(fun_str, 
                       expr_chars,
                       var_names, 
                       index = c(EVAL_DATA_INDEX, STORAGE_INDEX, DEFAULT_INDEX, LOOP_ITER_VARS), 
                       type = c("data", "ref"), 
                       var_mapping = c(GPU_MAPPING, CPU_MAPPING, GPU_INTERMEDIATE_EVAL_MAPPING,
                                       CPU_INTERMEDIATE_EVAL_MAPPING),
                       depth = 0,
                       allocate_intermediate_exprs = TRUE
) {
  
  # Match args
  type <- match.arg(type)
  index <- match.arg(index)
  var_mapping <- match.arg(var_mapping)
  
  args_start <- nchar(fun_str) + 2
  args <- identify_args(substr(expr_chars, args_start, nchar(expr_chars)))
  parsed_args <- list()
  additional_lines <- c()
  
  for (i in seq_along(args)) {
    
    # Check case where the argument is a void return type function call,
    # this means an intermediate evaluation will be needed as no value will
    # be directly returned by the parsed expression
    void_index <- which(startsWith(args[i], RAW_VOID_RET_FUNS))
    if (length(void_index)) {
      
      var_index <- which(args[i] == var_names)
      if (length(var_index)) ref_type <- RVAR_REF
      else ref_type <- INT_EVAL_REF
      
      # Identify whether CPU or GPU memory is requested, GPU parsing is called
      # first, and this is when intermediate allocations are requested, so if
      # no intermediate evaluation parsing is requested, type is CPU, else GPU
      if (allocate_intermediate_exprs) {
        
        intermediate_evaluations <- get_intermediate_evaluation(args[i], var_names)
        additional_lines <- c(additional_lines, intermediate_evaluations)
        
        # Identify the correct GPU mapping, whether it is global Rvars or 
        # intermediate evaluations, and also identify correct index to access 
        # the specific Rvar that is required
        mapping <- paste(GPU_PARSING, ref_type, sep = "_")
        if (!length(var_index)) var_index <- g_int_eval_env$count
        
        # TRANSLATE VARIABLE RATHER THAN GET REF SINCE VECTOR MATH
        # IS PARENT FUNCTION
        parsed_args[[i]] <- translate_variable(var_index, var_mapping=mapping)
      }
      else {
        
        # Get the compiled code array name of the selected reference type
        mapping <- paste(CPU_PARSING, ref_type, sep = "_")
        
        # Parse the argument using the identified mapping, in this case the only
        # thing returned from parse_expr will be a Rvar ref with memory type 
        # dependent on the mapping generated in this function
        parsed_args[[i]] <- parse_expr(args[i], var_names=var_names,
                                       index=index, type='ref',
                                       var_mapping=mapping, depth=depth,
                                       allocate_intermediate_exprs=FALSE)
      }
    }
    else {
      parsed_args[[i]] <- parse_expr(args[i], var_names=var_names, depth=depth,
                                     index=index, allocate_intermediate_exprs=allocate_intermediate_exprs)
      additional_lines <- c(additional_lines, get_additional_lines(parsed_args[i]))
    }
  }
  
  cur_args <- sapply(parsed_args, function(vec) { vec[length(vec)] })
  
  return(list(additional_lines=additional_lines, cur_args=cur_args))
}