# Package structure files updates
PACKAGE_LINE <- 1
TITLE_LINE <- 3
DYN_LIB_LINE <- 1
MAKEVARS_ALL_LINE <- 50
MAKEVARS_SO_LINE <- 52

# RcppExports.cpp updates
RCPP_FILE_NAME <- "RcppExports.cpp"
RCPP_EXP_LINES <- c(12, 21, 32)
RCPP_ARGS <- list(c(), c("SEXP varSEXP", "SEXP dimensionsSEXP"),
                  c("SEXP indexSEXP"))
RCPP_CALL_LINES <- c(44, 45, 46)
RCPP_INIT_LINE <- 50
RCPP_FUN_INDEX <- 3
RCPP_FUN_NAMES <- c("execute_commands", "bind_var", "write_data")

# RcppExports.R file updates
R_FILE_NAME <- "RcppExports.R"
R_FUN_LINES <- c(2, 6, 17)
R_FUN_NAMES <- c("execute_commands", "bind_vars", "update_vars")
R_FUN_INDENTS <- c(1, 2, 2)
R_ARGS <- list(c(), c("numeric_var", "dimension"), c("i"))
R_ASSIGN_TO <- c("", "", "data_vec <- ")
R_CALL_LINES <- c(3, 13, 19)


# Top level function called to update the meta information necessary
# to build the pseudo package
update_build <- function(cur_key, pkg_dir) {
  pure_key <- substr(cur_key, start = 4, stop = nchar(cur_key))
  new_key <- paste0("CCx", update_key(pure_key))
  description_file <- file.path(pkg_dir, "DESCRIPTION")
  namespace_file <- file.path(pkg_dir, "NAMESPACE")
  exports_file <- file.path(pkg_dir, "src", RCPP_FILE_NAME)
  R_file <- file.path(pkg_dir, "R", R_FILE_NAME)
  makevars_file <- file.path(pkg_dir, "src", "Makevars")
  update_description(new_key, description_file)
  update_namespace(new_key, namespace_file)
  update_makevars(new_key, makevars_file)
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
  file_contents <- readLines(file_loc)
  
  for (i in seq_along(RCPP_FUN_NAMES)) {
    dll_call_pattern <- paste0("_", cur_key, "_", RCPP_FUN_NAMES[i])
    rcpp_args <- RCPP_ARGS[[i]]
    updated_line <- paste("RcppExport", "SEXP", 
                          paste0(dll_call_pattern, "(", 
                                 paste0(rcpp_args, collapse = ", "),
                                 ")"),
                          "{")
    file_contents[RCPP_EXP_LINES[i]] <- updated_line
    updated_line <- paste0("    {\"", dll_call_pattern, "\", (DL_FUNC) &", 
                           dll_call_pattern, ", ", 
                           as.character(length(rcpp_args)), "},")
    file_contents[RCPP_CALL_LINES[i]] <- updated_line
  }
  file_contents[RCPP_INIT_LINE] <- paste0("RcppExport void R_init_", cur_key, 
                                          "(DllInfo *dll) {")
  writeLines(file_contents, file_loc)
}

update_R <- function(cur_key, file_loc) {
  file_contents <- readLines(file_loc)
  
  for (i in seq_along(R_FUN_NAMES)) {
    R_call_pattern <- paste0(cur_key, "_", R_FUN_NAMES[i])
    Cpp_call_pattern <- paste0("_", cur_key, "_", RCPP_FUN_NAMES[i])
    R_fun_line <- R_FUN_LINES[i]
    R_call_line <- R_CALL_LINES[i]
    R_args <- R_ARGS[[i]]
    updated_line <- gsub("(.*) <- function\\((.*)\\) \\{", 
                         paste0(R_call_pattern, ' <- function(\\2) {'),
                         file_contents[R_fun_line])
    file_contents[R_fun_line] <- updated_line
    file_contents[R_call_line] <- paste0(paste0(rep("  ", R_FUN_INDENTS[i]), 
                                                collapse = ""),
                                         R_ASSIGN_TO[i],
                                         "invisible(.Call(",
                                         paste(c(paste0("`", Cpp_call_pattern,
                                                        "`"),
                                                 R_args), collapse = ", "),
                                         "))")
  }
  
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

# Update the Makevars file of pseduo package
update_makevars <- function(cur_key, file_loc) {
  file_contents <- readLines(file_loc)
  file_contents[MAKEVARS_ALL_LINE] <- paste0("all : ", cur_key, ".so")
  file_contents[MAKEVARS_SO_LINE] <- paste0(cur_key, ".so: \$(OBJECTS)")
  writeLines(file_contents, file_loc)
}
