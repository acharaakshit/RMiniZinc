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
//' @param datafile the path of the dzn file.
// [[Rcpp::export]]
NumericVector mzn_eval(std::string modelString, std::string solver, std::string libpath,
                     std::string datafile = ""){
  //This is under development
  std::stringstream sol_strn;
  string sol_string;
  try {
    MznSolver slv(sol_strn,std::cerr);
    vector<std::string> options({"--stdlib-dir", libpath, "--solver", solver});
    if(datafile.length())
      options.push_back(datafile);
    slv.run(options,modelString, "minizinc", "xyz.mzn");
    sol_string = sol_strn.str();
  }catch (const LocationException& e) {
    sol_string = e.loc().toString();
    sol_string.append(":");
    sol_string.append(e.what());
    sol_string.append(e.msg());
  }catch (const Exception& e) {
    std::string what = e.what();
    sol_string  = what; 
    sol_string.append((what.empty() ? "" : ": "));
    sol_string.append(e.msg());
  }catch (const std::exception& e) {
    sol_string = e.what();
  }catch (...) {
    sol_string = "  UNKNOWN EXCEPTION." ;
  }
  return sol_parse(sol_string);
}




