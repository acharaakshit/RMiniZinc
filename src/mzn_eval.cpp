#include "config.h"
#include "helper_parse.h"
#include "sol_parse.h"
#include <fstream>

using namespace Rcpp;

//' @title MiniZinc model evaluation
//' 
//' @description evaluates the MiniZinc model
//'
//' @importFrom Rcpp sourceCpp
//' @export mzn_eval
//' @useDynLib rminizinc, .registration=TRUE
//' @param lib_path the path of the library where the solver is present.
//' @param r_model R6 Model object
//' @param mzn_path path of the mzn file to be solved
//' @param model_string model string to be solved.
//' @param solver the name of the solver to use.
//' @param dzn_path path of the datafile to be used.
//' @param all_solutions bool to specify if all solutions are specified.
//' @param time_limit stop after <time_limit> milliseconds
// [[Rcpp::export]]
List mzn_eval(std::string lib_path = "", 
              Rcpp::Nullable<Rcpp::Environment> r_model = R_NilValue,
              std::string mzn_path = "",
              std::string model_string = "",
              std::string solver = "org.gecode.gecode",
              std::string dzn_path = "",
              bool all_solutions = true, int time_limit = 300000);

#ifdef MZN_EVAL

#include <minizinc/solver.hh>

using namespace std;
using namespace MiniZinc;

std::string solver_file_map(std::string solconf, std::string solver_name){
  
  if(solver_name == "org.gecode.gecode" ||  solver_name == "Gecode"){
    return  solconf.append("/minizinc/solvers/gecode.msc");
  }else if(solver_name == "org.chuffed.chuffed" ||  solver_name == "Chuffed"){
    return solconf.append("/minizinc/solvers/chuffed.msc");
  }else if(solver_name == "org.gecode.gist" ||  solver_name == "Gecode Gist"){
    return solconf.append("/minizinc/solvers/gecode-gist.msc");
  }else if(solver_name == "org.minizinc.findmus" ||  solver_name == "findMUS"){
    return solconf.append("/minizinc/solvers/findmus.msc");
  }else{
    Rcpp::stop("Either the solver is not supported or incorrect solver_name is provided");
  }
  
}

// check if mznpath or model string is passed and take appropriate action
std::string envPathStringcheck(Rcpp::Nullable<Rcpp::Environment> r_model = R_NilValue,
                               std::string mznpath = "", std::string modelString = ""){
  if(mznpath.length() && modelString.empty() && r_model.isNull()){
    // check file extension
    if(!(mznpath.substr(mznpath.find_last_of(".") + 1) == "mzn" ))
      Rcpp::stop("file extention is not mzn");
    //convert to string 
    modelString = filetoString(mznpath);
  }else if(r_model.isNotNull() && mznpath.empty() && modelString.empty()){
    Environment rModel = Rcpp::as<Environment>(r_model);
    Function getModelString = rModel["mzn_string"];
    modelString  = Rcpp::as<string>(getModelString());
  }else if(r_model.isNull() && mznpath.empty() && modelString.length()){
    // do nothing
  }else{
    Rcpp::stop("PROVIDE ONLY ONE OF model_string OR mzn_path OR r_model");
  }
  return (modelString);
}


List mzn_eval(std::string lib_path, Nullable<Environment> r_model,
              std::string mzn_path,
              std::string model_string,
              std::string solver,
              std::string dzn_path,
              bool all_solutions, int time_limit){
  
  model_string = envPathStringcheck(r_model, mzn_path, model_string);
  Rcpp::Environment utils("package:utils");
  Rcpp::Function utils_cpp = utils["data"];
  Rcpp::Environment base("package:base");
  Rcpp::Function base_cpp = base[".libPaths"];
  List cv = Rcpp::as<List>(base_cpp()); 
  string solconf = cv[0];
  solconf = solconf.append("/rminizinc");
  solver = solver_file_map(solconf, solver);
  std::stringstream sol_strn;
  string sol_string;
  
  try {
    MznSolver slv(sol_strn, Rcpp::Rcerr);
    if(lib_path.empty())
    {
      solconf.append("/minizinc");
      lib_path = solconf;
    }
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

List mzn_eval(std::string lib_path, Nullable<Environment> r_model,
              std::string mzn_path,
              std::string model_string,
              std::string solver,
              std::string dzn_path,
              bool all_solutions, int time_limit){
  Rcpp::stop("Please install libminizinc (2.5.2) on your system and provide solver binaries!");
}

#endif


