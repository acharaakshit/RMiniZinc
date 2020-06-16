#include <Rcpp.h>
#include "sol_parse.h"
#include <minizinc/solver.hh>

using namespace std;
using namespace MiniZinc;
using namespace Rcpp;

//' @title MiniZinc model evaluation
//' 
//' @description evaluates the MiniZinc model
//'
//' @importFrom Rcpp sourceCpp
//' @export mzn_eval
//' @useDynLib rminizinc, .registration=TRUE
//' @param modelString the string representation of the model to be evaluated.
//' @param solver the name of the solver to use.
//' @param libpath the path of the library where the solver is present.
//' @param dznpath path of the datafile to be used
// [[Rcpp::export]]
NumericVector mzn_eval(std::string modelString, std::string solver, std::string libpath,
                       std::string dznpath = ""){
  
  std::stringstream sol_strn;
  string sol_string;
  try {
    MznSolver slv(sol_strn,std::cerr);
    vector<std::string> options({"--stdlib-dir", libpath, "--solver", solver});
    if(!dznpath.empty()) options.push_back(dznpath);
    slv.run(options,modelString, "minizinc", "model.mzn");
    sol_string = sol_strn.str();
  }catch (const LocationException& e) {
    string evalError = e.loc().toString();
    evalError.append(": ");
    evalError.append(e.what());
    evalError.append(" ");
    evalError.append(e.msg());
    Rcpp::stop(evalError);
  }catch (const Exception& e) {
    std::string what = e.what();
    string evalError  = what; 
    evalError.append((what.empty() ? " " : ": "));
    evalError.append(e.msg());
    Rcpp::stop(evalError);
  }catch (const std::exception& e) {
    Rcpp::stop(e.what());
  }catch (...) {
    Rcpp::stop("  UNKNOWN EXCEPTION.") ;
  }
  return sol_parse(sol_string);
}




