#include <Rcpp.h>
#include <regex>
#include "helper_parse.h"
#include "helper_sol_parse.h"


using namespace Rcpp;
using namespace MiniZinc;
using namespace std;

//' @title parse the solution
//' 
//' @description can parse the solution of a model
//' 
//' @importFrom Rcpp sourceCpp
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
    
    List thisSol;
    solutionString = solutions[nsol];
    CharacterVector varName;

    Model *model = helper_parse(solutionString, "solution.mzn");

    vector<Item*> items;
    
    for(int i=0; i < model->size(); i++){
      items.push_back(model->operator[] (i));
      bool isAssignment = (int)items[i]->iid()==Item::II_ASN?true:false;
      if(isAssignment){
        varName.push_back(items[i]->cast<AssignI>()->id().str());
        Expression *assignExp = items[i]->cast<AssignI>()->e();
        helper_sol_parse(assignExp, thisSol);
        
      }else {
        Rcpp::stop("Solution string contains non assignments");
      }
      thisSol.names() = varName;
    }
    string track_nsol = "solution:";
    track_nsol.append(to_string(nsol));
    if(nsol == solutions.size()-1 && optimal_sol_flag){
      track_nsol = "optimal_solution";
    }else if(nsol == solutions.size()-1){
      track_nsol = "best_solution";
    }
    retVal.push_back(thisSol);
    nameretVal.push_back(track_nsol);
  }
  retVal.names() = nameretVal;
  
  return retVal;
}