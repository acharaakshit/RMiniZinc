#include <Rcpp.h>
#include <regex>


using namespace Rcpp;
using namespace std;

//' @title parse the solution
//' 
//' @description can parse the solution of a model
//' 
//' @importFrom Rcpp sourceCpp
//' @import rjson
//' @export sol_parse
//' @useDynLib rminizinc, .registration=TRUE
//' @param solutionString solution of the model as a string representation
// [[Rcpp::export]]
List sol_parse(std::string solutionString) {
  
  string delimiter = "----------";
  size_t pos = 0;
  static const size_t npos = -1;
  std::string token;
  vector<string> solutions;
  
  while ((pos = solutionString.find(delimiter)) != npos) {
    token = solutionString.substr(0, pos);
    solutions.push_back(token);
    solutionString.erase(0, pos + delimiter.length());
  }
  
  if(solutionString.find("=====UNSATISFIABLE=====") != npos){
    Rcpp::stop("No Solution");
  }else if(solutionString.find("=====ERROR=====") != npos){
    Rcpp::stop("Errored");
  }else if(solutionString.find("=====UNKNOWN=====") != npos){
    Rcpp::stop("Unknown solution");
  }else if(solutionString.find("=====UNBOUNDED=====") != npos){
    Rcpp::stop("Unbounded solution");
  }else if(solutionString.find("=====UNSATorUNBOUNDED=====") != npos){
    Rcpp::stop("Unsatisfiable or Unbounded solution");
  }
  
  if(solutions.size()==0) Rcpp::stop("No solution seperator found-- incorrect solution string");
  
  int optimal_sol_flag = solutionString.find("==========") != npos ? 1:0;
  
  
  List retVal;
  CharacterVector nameretVal;
  
  for(int nsol=0; nsol< solutions.size(); nsol++){
    
    solutionString = solutions[nsol];
    Rcpp::Environment rjson("package:rjson");
    Rcpp::Function fromJSON_cpp = rjson["fromJSON"];  
    retVal.push_back(fromJSON_cpp(solutionString));
    string track_nsol = "SOLUTION";
    track_nsol.append(to_string(nsol));
    if(nsol == solutions.size()-1 && optimal_sol_flag){
      track_nsol = "OPTIMAL_SOLUTION";
    }else if(nsol == solutions.size()-1){
      track_nsol = "BEST_SOLUTION";
    }
    nameretVal.push_back(track_nsol);
  }
  retVal.names() = nameretVal;
  
  return retVal;
}