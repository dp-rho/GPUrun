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
// bind_var
void bind_var(NumericVector var, NumericVector dimensions);
RcppExport SEXP _CCxAAAA_bind_var(SEXP varSEXP, SEXP dimensionsSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type var(varSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type dimensions(dimensionsSEXP);
    bind_var(var, dimensions);
    return R_NilValue;
END_RCPP
}
// get_data
NumericVector get_data(int index);
RcppExport SEXP _CCxAAAA_get_data(SEXP indexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type index(indexSEXP);
    rcpp_result_gen = Rcpp::wrap(get_data(index));
    return rcpp_result_gen;
END_RCPP
}


static const R_CallMethodDef CallEntries[] = {
    {"_CCxAAAA_execute_commands", (DL_FUNC) &_CCxAAAA_execute_commands, 0},
    {"_CCxAAAA_bind_var", (DL_FUNC) &_CCxAAAA_bind_var, 2},
    {"_CCxAAAA_get_data", (DL_FUNC) &_CCxAAAA_get_data, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_CCxAAAA(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
