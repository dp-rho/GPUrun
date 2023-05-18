
# .h files can be in original package download, included at compile time
# 
# > compile_commands(expr_ls)
#   create kernel.cu
#   write variable information to __constant__ memory in kernel.cu
#   parse commands into recursive expr structures in current library
#   write expr structures to .cu file in kernel call
#   compile kernel.cu with prebuilt execute_commands.cpp
#   call dyn.load(parsed_exprs.so)
# 
# > execute_commands()
#   .Call("execute_commands")