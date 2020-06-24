#include <Rcpp.h>
#include "helper_parse.h"
#include "filetoString.h"

using namespace Rcpp;
using namespace std;
using namespace MiniZinc;

//' @title get missing parameters
//' 
//' @description get the names of missing parameters in a model
//' 
//' @importFrom Rcpp sourceCpp
//' @export getMissingPars
//' @useDynLib rminizinc, .registration=TRUE
//' @param modelString the string representation of a MiniZinc model
//' @param mznpath the path of the MiniZinc model mzn file
//' @param modelStringName the custom name of the mzn string
// [[Rcpp::export]]
Rcpp::CharacterVector getMissingPars(std::string modelString="",
                                     std::string mznpath="",
                                     std::string modelStringName="missing_pars.mzn"){
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
  
  Model *model = helper_parse(modelString, modelStringName);  
  
  int s = model-> size();
  if(s == 0){
    Rcpp::stop("Empty model!");
  }
  
  CharacterVector missingPars;
  vector<Item*> items;
  for(int i=0; i < s; i++){
    items.push_back(model->operator[] (i));
    if(items[i]->iid() == Item::II_VD){
      string name = items[i]->cast<VarDeclI>()->e()->id()->str().aststr()->c_str();
      // decision variables or parameters
      if(items[i]->cast<VarDeclI>()->e()->e() == NULL &&  items[i]->cast<VarDeclI>()->e()->type().ispar()){
        // uninitialized parameter
        missingPars.push_back(name);
      }
    }else if (items[i]->iid() == Item::II_ASN){
      // assignment of the parameters
      int index = missingPars.offset(items[i]->cast<AssignI>()->id().str());
      missingPars.erase(index);
    }
  }    
    return missingPars;
}
  
  
