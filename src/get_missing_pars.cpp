#include "helper_parse.h"

using namespace Rcpp;
using namespace std;
using namespace MiniZinc;

//' @title get missing parameters
//' 
//' @description get the names of missing parameters in a model
//' 
//' @importFrom Rcpp sourceCpp
//' @export get_missing_pars
//' @useDynLib rminizinc, .registration=TRUE
//' @param modelString the string representation of a MiniZinc model
//' @param mznpath the path of the MiniZinc model mzn file
//' @param modelStringName the custom name of the mzn string
//' @param includePath path of the included mzn in the model if it exists.
// [[Rcpp::export]]
Rcpp::CharacterVector get_missing_pars(std::string modelString = "",
                                     std::string mznpath = "",
                                     std::string modelStringName = "missing_pars.h",
                                     Nullable<std::vector<std::string>> includePath = R_NilValue){
  
  modelString  = pathStringcheck(modelString, mznpath);
  vector<string> ip;
  if(!Rf_isNull(includePath)){
    ip = Rcpp::as<vector<string>>(includePath);
  }
  Model *model = helper_parse(modelString, modelStringName, ip);  

  CharacterVector missingPars;
  NumericVector indexPars;
  vector<Item*> items;
  
  for(int i=0; i < model->size(); i++){
    items.push_back(model->operator[] (i));
    
    if(items[i]->iid() == Item::II_VD){
      string name = items[i]->cast<VarDeclI>()->e()->id()->str().aststr()->c_str();
      // decision variables or parameters
      if(items[i]->cast<VarDeclI>()->e()->e() == NULL &&  items[i]->cast<VarDeclI>()->e()->type().ispar()){
        // uninitialized parameter
        indexPars.push_back(i);
        missingPars.push_back(name);
        indexPars.names() = missingPars;
      }
    }else if(items[i]->iid() == Item::II_ASN){
      // assignment of the parameters
      int index;
      try{
        index  = indexPars.offset(items[i]->cast<AssignI>()->id().str()); 
        indexPars.erase(index);
        missingPars.erase(index);
      }catch(std::exception &e){
        Rcpp::stop(e.what());
      }
    }
  }    
    return missingPars;
}
  
  
