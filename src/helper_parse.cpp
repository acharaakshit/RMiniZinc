#include "helper_parse.h"
#include <Rcpp.h>

using namespace std;
using namespace MiniZinc;
using namespace Rcpp;

MiniZinc::Model* helper_parse(std::string modelString, std::string modelStringName){
  Env* env = new Env();
  // include paths of mzn files to be included
  vector<string> ip = {"/usr/local/lib/libminizinc/share/minizinc/std"};
  ostringstream os;
  vector<SyntaxError> se;
  Model *model;
  try{
    model = MiniZinc::parseFromString(*env, modelString, modelStringName , ip, true, true, true, os, se);
    if(model==NULL) throw std::exception();
    else if(se.size()){
      string syntaxErrors;
      for(int i = 0;i < se.size();i++){
        syntaxErrors.append(se[i].what());
        syntaxErrors.append("\n");
      }
      Rcpp::stop(syntaxErrors);
    }
  }catch(std::exception& e){
    string parseError;
    parseError = os.str();
    Rcpp::stop(parseError);
  } 
  return model;
}