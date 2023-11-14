
write_linalg_dims <- function(kernel_lines) {
  linalg_dims <- paste(g_linalg_env$linalg_dims, collapse = ", ")
  for (i in seq_along(kernel_lines)) {
    if (grepl("/* R::linalg_dim */", kernel_lines[i], fixed = TRUE)) {
      kernel_lines[i] <- gsub("/* R::linalg_dim */", linalg_dims, 
                              kernel_lines[i], fixed = TRUE)
    }
  }
  return(kernel_lines)
}