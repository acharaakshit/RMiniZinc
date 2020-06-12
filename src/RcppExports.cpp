// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// mzn_eval
std::string mzn_eval(std::string modelString, std::string solver, std::string libpath, std::string datafile);
RcppExport SEXP _rminizinc_mzn_eval(SEXP modelStringSEXP, SEXP solverSEXP, SEXP libpathSEXP, SEXP datafileSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type modelString(modelStringSEXP);
    Rcpp::traits::input_parameter< std::string >::type solver(solverSEXP);
    Rcpp::traits::input_parameter< std::string >::type libpath(libpathSEXP);
    Rcpp::traits::input_parameter< std::string >::type datafile(datafileSEXP);
    rcpp_result_gen = Rcpp::wrap(mzn_eval(modelString, solver, libpath, datafile));
    return rcpp_result_gen;
END_RCPP
}
// mzn_parse
NumericVector mzn_parse(std::string modelString, std::string modelStringName, std::vector<std::string> mznfilename, std::vector<std::string> dznfilename);
RcppExport SEXP _rminizinc_mzn_parse(SEXP modelStringSEXP, SEXP modelStringNameSEXP, SEXP mznfilenameSEXP, SEXP dznfilenameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type modelString(modelStringSEXP);
    Rcpp::traits::input_parameter< std::string >::type modelStringName(modelStringNameSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type mznfilename(mznfilenameSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type dznfilename(dznfilenameSEXP);
    rcpp_result_gen = Rcpp::wrap(mzn_parse(modelString, modelStringName, mznfilename, dznfilename));
    return rcpp_result_gen;
END_RCPP
}
// sol_parse
NumericVector sol_parse(std::string solutionString);
RcppExport SEXP _rminizinc_sol_parse(SEXP solutionStringSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type solutionString(solutionStringSEXP);
    rcpp_result_gen = Rcpp::wrap(sol_parse(solutionString));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_rminizinc_mzn_eval", (DL_FUNC) &_rminizinc_mzn_eval, 4},
    {"_rminizinc_mzn_parse", (DL_FUNC) &_rminizinc_mzn_parse, 4},
    {"_rminizinc_sol_parse", (DL_FUNC) &_rminizinc_sol_parse, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_rminizinc(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
