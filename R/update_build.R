
# Increments a character key of the form {XXXX} to {XXXY}
update_key <- function(cur_key) {
  ptm <- proc.time()
  numeric_vector <- utf8ToInt(cur_key)
  index <- length(numeric_vector)
  numeric_vector[index] <- numeric_vector[index] + 1
  while (numeric_vector[index] > 90) {
    numeric_vector[index] <- 65
    index <- if (index != 1) index - 1 else length(numeric_vector)
    if (index == length(numeric_vector)) break
    numeric_vector[index] <- numeric_vector[index] + 1
  }
  print(proc.time() - ptm)
  return(intToUtf8(numeric_vector))
}

# Update the description 
update_description <- function(cur_key, file_loc) {
  
}