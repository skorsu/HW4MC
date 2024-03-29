// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// tg_dist_ind
double tg_dist_ind(double x, double a, double b);
RcppExport SEXP _HW4MC_tg_dist_ind(SEXP xSEXP, SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(tg_dist_ind(x, a, b));
    return rcpp_result_gen;
END_RCPP
}
// tg_dist
arma::vec tg_dist(arma::vec x, double a, double b);
RcppExport SEXP _HW4MC_tg_dist(SEXP xSEXP, SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(tg_dist(x, a, b));
    return rcpp_result_gen;
END_RCPP
}
// isRcpp
arma::mat isRcpp(unsigned int n, double a, double b, double df);
RcppExport SEXP _HW4MC_isRcpp(SEXP nSEXP, SEXP aSEXP, SEXP bSEXP, SEXP dfSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< unsigned int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    Rcpp::traits::input_parameter< double >::type df(dfSEXP);
    rcpp_result_gen = Rcpp::wrap(isRcpp(n, a, b, df));
    return rcpp_result_gen;
END_RCPP
}
// rsRcpp
arma::vec rsRcpp(unsigned int n, double alp, double a, double b, double df);
RcppExport SEXP _HW4MC_rsRcpp(SEXP nSEXP, SEXP alpSEXP, SEXP aSEXP, SEXP bSEXP, SEXP dfSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< unsigned int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type alp(alpSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    Rcpp::traits::input_parameter< double >::type df(dfSEXP);
    rcpp_result_gen = Rcpp::wrap(rsRcpp(n, alp, a, b, df));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_HW4MC_tg_dist_ind", (DL_FUNC) &_HW4MC_tg_dist_ind, 3},
    {"_HW4MC_tg_dist", (DL_FUNC) &_HW4MC_tg_dist, 3},
    {"_HW4MC_isRcpp", (DL_FUNC) &_HW4MC_isRcpp, 4},
    {"_HW4MC_rsRcpp", (DL_FUNC) &_HW4MC_rsRcpp, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_HW4MC(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
