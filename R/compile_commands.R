require(Rcpp)
require(devtools)

PSEUDO_NAME <- "buildDyn"
INSTALL_LOC <- "installed_temp"
DESCRIPTION_LINE <- 1

# Compiles a list of expressions into a .so binary that can called
# in the current R session by using the object returned from this
# function call
compile_commands <- function(expr_ls) {
  
  # get the list of variable names in an ordered list
  # to allow machine generated code to access them by index
  # var_names <- get_ordered_vars(expr_ls)
  
  # write machine generated code to .cu file that will 
  # compiled with included .cpp files
  # write_kernel(expr_ls, var_names)
  
  # get location of compile directory
  compile_path <- system.file("compile", package = "GPUrun")
  
  # get location of pseudo package called PSEUDO_NAME
  pseudo_pkd_dir <- file.path(compile_path, PSEUDO_NAME)
  
  # get current key of pseudo package
  description_text <- base::readLines(file.path(pseudo_pkd_dir, "DESCRIPTION"))
  current_pkg_key <- sub("Package: ", "\\1", description_text[DESCRIPTION_LINE])
  
  # get location of where the pseudo package will be installed to
  install_loc <- file.path(compile_path, INSTALL_LOC)
  
  # use devtools to build and install the pseudo package package
  # in order to generate a portable binary .so object
  devtools::install(pkg = pseudo_pkd_dir, 
                    args = paste0("--library=", install_loc))
  
  # get location of compiled binary shared library object
  binary_file <- file.path(pseudo_pkd_dir, "src", 
                           paste0(current_pkg_key, ".so"))
  
  # load the binary into the current R session
  dyn.load(binary_file)
  
  # remove the temporary library that was installed
  unlink(file.path(install_loc, current_pkg_key), recursive = TRUE)
  
  # increment the key values in package meta files
  pure_key <- substr(current_pkg_key, start = 4, stop = nchar(current_pkg_key))
  pure_key <- update_key(pure_key)
  
}