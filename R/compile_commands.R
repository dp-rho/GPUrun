PSEUDO_NAME <- "buildDyn"
INSTALL_LOC <- "compiled_commands"
RLIBS <- "libs"

#' @title Parse R expressions into parallel CUDA code and compile
#' 
#' @description
#' Parses the list of input R expressions into CUDA code, including dimensional
#' information executed on CPU prior to GPU device call. Also compiles the 
#' generated machine code into a pseudo R package that can be called using
#' run_commands.
#' 
#' @param expr_ls A character string representing the unparsed expression
#' for which the dimensions are to be identified.
#' @param quiet A boolean which controls whether pseudo package building is
#' displayed.
#' 
#' @returns A commands objects that holds the meta information necessary for
#' executing the compiled commands from a specified environment.
#' @examples
#' expr_ls <- list(substitute(x <- x + 1))
#' 
#' commands_object <- compile_commands(expr_ls)
#' 
#' run_commands(commands_object, environment())
compile_commands <- function(expr_ls, quiet=TRUE) {
  
  # Libraries which define functions that are implemented in GPUrun
  # or are required to install the .so libs
  library(truncnorm)
  library(MASS)
  library(usethis)
  library(devtools)

  # get the list of variable names in an ordered list
  # to allow machine generated code to access them by index
  var_names <- unique(unlist(lapply(expr_ls, all.vars)))

  # write machine generated code to .cu file that will 
  # compiled with included .cpp files
  interpreter(expr_ls, var_names)
  
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
                    quiet = quiet,
                    args = paste0("--library=", install_loc))
  
  # increment the key values in package meta files
  update_build(current_pkg_key, pseudo_pkg_dir)
  
  # Return the object that contains all necessary information to call
  # the compiled code
  return(
    list(
      exprs = expr_ls,
      key = current_pkg_key,
      vars = var_names
    )
  )
}
