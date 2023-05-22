PACKAGE_LINE <- 1
TITLE_LINE <- 3
DYN_LIB_LINE <- 1
RCPP_EXP_LINE <- 12
RCPP_CALL_LINE <- 21
RCPP_INIT_LINE <- 25
RCPP_FUN_INDEX <- 3
RCPP_FILE_NAME <- "RcppExports.cpp"
RCPP_FUN_NAME <- "execute_commands"
R_FUN_LINE <- 2
R_CALL_LINE <- 3
R_FILE_NAME <- "RcppExports.R"


# Top level function called to update the meta information necessary
# to build the pseudo package
update_build <- function(cur_key, pkg_dir) {
  pure_key <- substr(cur_key, start = 4, stop = nchar(cur_key))
  new_key <- paste0("CCx", update_key(pure_key))
  description_file <- file.path(pkg_dir, "DESCRIPTION")
  namespace_file <- file.path(pkg_dir, "NAMESPACE")
  exports_file <- file.path(pkg_dir, "src", RCPP_FILE_NAME)
  R_file <- file.path(pkg_dir, "R", R_FILE_NAME)
  update_description(new_key, description_file)
  update_namespace(new_key, namespace_file)
  update_exports(new_key, exports_file)
  update_R(new_key, R_file)
}


# Increments a character key of the form {XXXX} to {XXXY}
update_key <- function(cur_key) {
  numeric_vector <- utf8ToInt(cur_key)
  index <- length(numeric_vector)
  numeric_vector[index] <- numeric_vector[index] + 1
  while (numeric_vector[index] > 90) {
    numeric_vector[index] <- 65
    index <- if (index != 1) index - 1 else length(numeric_vector)
    if (index == length(numeric_vector)) break
    numeric_vector[index] <- numeric_vector[index] + 1
  }
  return(intToUtf8(numeric_vector))
}

# Update the RcppExports.cpp file that names the function callable by
# the .Call interface in the R session
update_exports <- function(cur_key, file_loc) {
  dll_call_pattern <- paste0("_", cur_key, "_", RCPP_FUN_NAME)
  file_contents <- readLines(file_loc)
  updated_line <- gsub("^((\\S+\\s+){2})(\\S+)(.*)$", 
                       paste0("\\1", dll_call_pattern, "()\\4"),
                       file_contents[RCPP_EXP_LINE])
  file_contents[RCPP_EXP_LINE] <- updated_line
  updated_line <- gsub("\\{\"(.*?)\", \\(DL_FUNC\\) &(.*?), 0\\}", 
                       paste0("{\\\"", dll_call_pattern, "\", (DL_FUNC) &", 
                              dll_call_pattern, ", 0}"),
                       file_contents[RCPP_CALL_LINE])
  file_contents[RCPP_CALL_LINE] <- updated_line
  file_contents[RCPP_INIT_LINE] <- paste0("RcppExport void R_init_", cur_key, 
                                          "(DllInfo *dll) {")
  writeLines(file_contents, file_loc)
}

update_R <- function(cur_key, file_loc) {
  R_call_pattern <- paste0(cur_key, "_", RCPP_FUN_NAME)
  file_contents <- readLines(file_loc)
  updated_line <- gsub("(.*) <- function\\(\\) \\{", 
                       paste0(R_call_pattern, ' <- function() {'),
                       file_contents[R_FUN_LINE])
  file_contents[R_FUN_LINE] <- updated_line
  file_contents[R_CALL_LINE] <- paste0("  invisible(.Call(`_", 
                                       R_call_pattern, "`))")
  writeLines(file_contents, file_loc)
}

# Update the DESCRIPTION file of pseudo package
update_description <- function(cur_key, file_loc) {
  file_contents <- readLines(file_loc)
  file_contents[PACKAGE_LINE] <- paste0("Package: ", cur_key)
  file_contents[TITLE_LINE] <- paste0("Title: ", cur_key)
  writeLines(file_contents, file_loc)
}

# Update the NAMESPACE file of pseudo package
update_namespace <- function(cur_key, file_loc) {
  file_contents <- readLines(file_loc)
  file_contents[DYN_LIB_LINE] <- paste0("useDynLib(", cur_key, ", .registration=TRUE)")
  writeLines(file_contents, file_loc)
}
