#include "commands.h"


/* Shell function used to call .cu compiled code through Rcpp interface */
// [[Rcpp::export]]
void execute_commands() {

  /* Implemented in kernel.cu  */
  call_device();
}
