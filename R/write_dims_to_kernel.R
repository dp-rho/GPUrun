
write_dims_to_kernel <- function(post_kernel_lines) {
  
  # Write the dimensions of the linear algebra evaluations
  post_kernel_lines <- write_parsed_dimensions(post_kernel_lines, g_linalg_env)
  
  # Write the dimensions of the intermediate evaluation Rvar structures to the
  # compiled initialization function in kernel.cu called initialize_int_evals()
  post_kernel_lines <- write_parsed_dimensions(post_kernel_lines, g_int_eval_env)
  post_kernel_lines <- replace_gpu_mem_access(post_kernel_lines, "Int.evals", "Int.mem")
  
  # Write the lengths of loop iterations to the compiled initialization function
  # in kernel.cu called initialize_iter_lens()
  post_kernel_lines <- write_parsed_dimensions(post_kernel_lines, g_loop_env)
  post_kernel_lines <- replace_gpu_mem_access(post_kernel_lines, "Iter.lens", "Iter.mem")
  
  # Write the expression lengths and by extension the number of evaluations per 
  # thread to the compiled initialization function in kernel.cu called
  post_kernel_lines <- write_parsed_dimensions(post_kernel_lines, g_expr_env)
  post_kernel_lines <- replace_gpu_mem_access(post_kernel_lines, "Expr.lens", "Expr.mem")
  
  return(post_kernel_lines)
}