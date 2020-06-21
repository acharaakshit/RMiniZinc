#include <Rcpp.h>
#include <minizinc/solver.hh>
#include "sol_parse.h"
#include "filetoString.h"

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
//' @param mznpath the path of the MiniZinc model file.
//' @param dznpath path of the datafile to be used.
//' @param all_solutions bool to specify if all solutions are specified.
// [[Rcpp::export]]
List mzn_eval(std::string solver, std::string libpath,std::string modelString = "", 
                       std::string mznpath = "", std::string dznpath = "",
                       bool all_solutions = true){
  
  if(modelString.empty() && mznpath.empty()){
    Rcpp::stop("PROVIDE EITHER modelString OR mznfilename");
  }else if(!modelString.empty() && !mznpath.empty()){
    Rcpp::stop("PROVIDE ONLY ONE OF modelString OR mznfilename");
  }else if(mznpath.length()){
    // check file extension
    if(!(mznpath.substr(mznpath.find_last_of(".") + 1) == "mzn" ))
      Rcpp::stop("file extention is not mzn");
    //convert to string 
    modelString = filetoString(mznpath);
  }
  std::stringstream sol_strn;
  string sol_string;
  if(solver != "Gecode" && solver != "org.gecode.gecode")
    Rcpp:stop("only Gecode solver is supported for now");
  try {
    MznSolver slv(sol_strn,std::cerr);
    vector<std::string> options({"--stdlib-dir", libpath, "--solver", solver});
    if(!dznpath.empty()) options.push_back(dznpath);
    if(all_solutions) options.push_back("-a");
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

  List retVal;
  retVal.push_back(sol_string);
  retVal.push_back(sol_parse(sol_string));
  retVal.names() = CharacterVector({"solutionString", "Solutions"});

  return retVal;
}




