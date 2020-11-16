#include "config.h"
#include "helper_parse.h"
#include "sol_parse.h"

using namespace Rcpp;

#ifdef MZN_3

#include <minizinc/solver.hh>

using namespace std;
using namespace MiniZinc;


//' @title MiniZinc model evaluation
//' 
//' @description evaluates the MiniZinc model
//'
//' @importFrom Rcpp sourceCpp
//' @export mzn_eval
//' @useDynLib rminizinc, .registration=TRUE
//' @param solver the name of the solver to use.
//' @param lib_path the path of the library where the solver is present.
//' @param r_model R6 Model object
//' @param dzn_path path of the datafile to be used.
//' @param all_solutions bool to specify if all solutions are specified.
//' @param time_limit stop after <time_limit> milliseconds
// [[Rcpp::export]]
List mzn_eval(std::string solver, std::string lib_path, Environment &r_model,
              std::string dzn_path = "",
              bool all_solutions = true, int time_limit = 300000){
  
  Function getModelString = r_model["mzn_string"];
  string model_string  = Rcpp::as<string>(getModelString());  
  std::stringstream sol_strn;
  string sol_string;
  if(solver != "Gecode" && solver != "org.gecode.gecode")
    Rcpp:stop("only Gecode solver is supported for now");
  try {
    MznSolver slv(sol_strn, Rcpp::Rcerr);
    vector<std::string> options({"--stdlib-dir", lib_path, "--solver", solver,
                                "--output-mode", "json", "--time-limit", to_string(time_limit)});
    if(!dzn_path.empty()) options.push_back(dzn_path);
    if(all_solutions) options.push_back("-a");
    slv.run(options,model_string, "minizinc", "model.mzn");
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
  
  List retVal;
  retVal.push_back(sol_string);
  
  try{
    retVal.push_back(sol_parse(sol_string));
    retVal.names() = CharacterVector({"SOLUTION_STRING", "SOLUTIONS"});
  }catch(std::exception &e){
    retVal.push_back(e.what());
    retVal.names() = CharacterVector({"SOLUTION_STRING", "SOLUTION_PARSE_ERROR"});
  }
  return retVal;
}

#else

List mzn_eval(std::string solver, std::string lib_path, Rcpp::Environment &r_model,
              std::string dzn_path = "",
              bool all_solutions = true, int time_limit = 300000){
stop("Please set up MiniZinc, libminizinc and re-install the package!");
}

#endif


