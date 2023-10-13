
# Replace all instances of either CPU or GPU specific code with 
# code from the other 
set_mem_type <- function(
  expr_chars,
  mem_type = c("cpu", "gpu")    
) {
  
  if (mem_type == "cpu") {
    expr_chars <- gsub(GPU_MAPPING, CPU_MAPPING, expr_chars)
    expr_chars <- gsub(GPU_INTERMEDIATE_EVAL_MAPPING, CPU_INTERMEDIATE_EVAL_MAPPING, expr_chars)
  }
  
  if (mem_type == "gpu") {
    expr_chars <- gsub(CPU_MAPPING, GPU_MAPPING, expr_chars)
    expr_chars <- gsub(CPU_INTERMEDIATE_EVAL_MAPPING, GPU_INTERMEDIATE_EVAL_MAPPING, expr_chars)
  }
  
  return(expr_chars)
}