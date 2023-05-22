#include "commands.h"

/* Function which calls the machine generated kernel code */
extern void call_device();

/* Shell function used to call .cu compiled code through Rcpp interface */
// [[Rcpp::export]]
void execute_commands() {

  /* Implemented in call_device.cu  */
	call_device();
}
