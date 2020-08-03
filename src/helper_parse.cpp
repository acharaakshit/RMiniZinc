#include "helper_parse.h"
#include <Rcpp.h>

using namespace std;
using namespace MiniZinc;
using namespace Rcpp;

MiniZinc::Model* helper_parse(std::string modelString, std::string modelStringName){
  Env* env = new Env();
  // include paths of mzn files to be included
  vector<string> ip = {"/usr/local/lib/libminizinc/share/minizinc/std"};
  vector<SyntaxError> se;
  Model *model;
  try{
    std::stringstream ss;
    //change the underlying buffer and save the old buffer
    auto old_buf = std::cerr.rdbuf(ss.rdbuf()); 
    model = MiniZinc::parseFromString(*env, modelString, modelStringName , ip, true, true, true, Rcpp::Rcerr, se);
    std::cerr.rdbuf(old_buf); //reset
    Rcerr << ss.str();
    if(model==NULL) throw std::exception();
    else if(model->size() == 0) Rcpp::stop("Empty Model!");
  }catch(std::exception& e){
    Rcpp::stop("NULL model !");
  }
  return model;
}