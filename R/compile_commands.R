require(Rcpp)
require(devtools)

PSEUDO_NAME <- "buildDyn"
INSTALL_LOC <- "installed_temp"
RLIBS <- "libs"


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
  pseudo_pkg_dir <- file.path(compile_path, PSEUDO_NAME)
  
  # get current key of pseudo package
  description_text <- base::readLines(file.path(pseudo_pkg_dir, "DESCRIPTION"))
  current_pkg_key <- sub("Package: ", "\\1", description_text[PACKAGE_LINE])
  
  # get location of where the pseudo package will be installed to
  install_loc <- file.path(compile_path, INSTALL_LOC)
  
  # use devtools to build and install the pseudo package package
  # in order to generate a portable binary .so object
  devtools::install(pkg = pseudo_pkg_dir, 
                    args = paste0("--library=", install_loc))
  
  # get location of compiled binary shared library object
  binary_file <- file.path(install_loc, current_pkg_key, RLIBS, 
                           paste0(current_pkg_key, ".so"))
  
  # copy the binary into the installed libs temporary directory
  file.copy(binary_file, file.path(install_loc, RLIBS))
  
  # remove the temporary library that was installed
  # unlink(file.path(install_loc, current_pkg_key), recursive = TRUE)
  
  # increment the key values in package meta files
  update_build(current_pkg_key, pseudo_pkg_dir)
  
  return(
    list(
      key = current_pkg_key
    )
  )
}


# Top level function to run compiled commands in the input envrionment
run_commands <- function(compiled_commands) {
  compile_path <- system.file("compile", package = "GPUrun")
  temp_path <- file.path(compile_path, INSTALL_LOC)
  library(compiled_commands$key, lib.loc = temp_path,
          character.only = T)
  eval(parse(text = paste0(compiled_commands$key, "_execute_commands()")))
  unloadNamespace(compiled_commands$key)
}