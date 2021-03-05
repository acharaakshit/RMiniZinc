#include "sol_parse.h"
#include <regex>


using namespace Rcpp;
using namespace std;

//' @title parse the solution
//' 
//' @description can parse the JSON solution of a model to 
//' return a list output
//' @importFrom Rcpp sourceCpp
//' @import rjson
//' @export sol_parse
//' @useDynLib rminizinc, .registration=TRUE
//' @param solutionString solution of the model as a string representation
// [[Rcpp::export]]
List sol_parse(std::string solutionString) {
  
  static const size_t npos = -1;
  
  // stop if no solution or similar
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
  
  // remove comments in the solution
  string str = solutionString;
  size_t nFPos = 0;     
  while(npos != (nFPos = solutionString.find('%')))
  {
      size_t second = solutionString.find('\n', nFPos);
      solutionString.erase(nFPos, second - nFPos + 1);
  }   
  
  // get the solutions using the delimiter
  string delimiter = "----------";
  size_t pos = 0;
  std::string token;
  vector<string> solutions;
  
  while ((pos = solutionString.find(delimiter)) != npos) {
    token = solutionString.substr(0, pos);
    solutions.push_back(token);
    solutionString.erase(0, pos + delimiter.length());
  }
  
  if(solutions.size()==0) {
    Rcpp::stop("No '----------' solution seperator found -- incorrect solution string or maybe --compile or similar was used which resulted in no output"); 
  }
  
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