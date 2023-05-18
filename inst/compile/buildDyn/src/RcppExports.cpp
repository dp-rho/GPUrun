#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// execute_commands
void execute_commands();
RcppExport SEXP _CCxAAAA_execute_commands() {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    execute_commands();
    return R_NilValue;
END_RCPP
}
