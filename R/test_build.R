setwd("~/dev/R_packages/GPUrun")
require(Rcpp)

test_compile_commands <- function() {
  # START TIME:
  ptm <- proc.time()
  
  # Create C++ file
  call_file <- file("inst/compile/buildDyn/src/execute_commands.cpp", "w")
  
  # lines <- c("#include \"commands.h\"",
  #            "extern void call_device();",
  #            "void execute_commands() {",
  #            "\tcall_device();",
  #            "}")
  # writeLines(lines, call_file)
  close(call_file)
  # Rcpp::compileAttributes(pkgdir = "./buildDyn")
  
  # system.file("buildDyn", package = 'GPUrun')
  invisible(system("R CMD build inst/compile/buildDyn inst/compile"))# > NUL 2>&1"))
  invisible(system("R CMD INSTALL compile/CCxAAAA_0.1.0.tar.gz --library=./inst/compile/buildDyn"))# > NUL 2>&1"))
  
  dyn.load("./inst/compile/buildDyn/CCxAAAA/libs/CCxAAAA.so")
  invisible(system("rm -rf ./inst/compile/buildDyn/CCxAAAA > NUL 2>&1"))
  # invisible(system("rm compile/CCxAAAA_0.1.0.tar.gz > NUL 2>&1"))
  
  print(proc.time() - ptm)
  print("Compile finished")
}




# invisible(.Call("_CCxAAAA_execute_commands"))
# dyn.unload()

