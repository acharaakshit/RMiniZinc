#include <Rcpp.h>
#include <minizinc/prettyprinter.hh>
#include "filetoString.h"
#include "helper_parse.h"

using namespace std;
using namespace MiniZinc;

using namespace Rcpp;

//' @title MiniZinc syntax parser
//' 
//' @description parses the MiniZinc syntax into R objects
//'
//' @importFrom Rcpp sourceCpp
//' @export mzn_parse
//' @useDynLib rminizinc, .registration=TRUE
//' @param modelString string representation of the MiniZinc model.
//' @param mznpath the path of model mzn.
//' @param modelStringName the name of model string.
// [[Rcpp::export]]
List mzn_parse(std::string modelString = "", 
                      std::string mznpath = "",
                      std::string  modelStringName = "mzn_parse.mzn"){
  
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
  
  List retVal;
  // size of the model
  int s = model-> size();
  if(s == 0){
    Rcpp::stop("Empty model!");
  }
  // get all the items and the names of all the parameters and map to the item numbers
  vector<Item*> items;
  int type = 0;
  // to store the missing parameter names
  CharacterVector missingPars;
  for(int i=0; i < s; i++){
    items.push_back(model->operator[] (i));
    if(items[i]->iid() == Item::II_VD){
      // decision variables or parameters
     if(items[i]->cast<VarDeclI>()->e()->e() == NULL &&  items[i]->cast<VarDeclI>()->e()->type().ispar()){
        // uninitialized parameter
        string name = items[i]->cast<VarDeclI>()->e()->id()->str().aststr()->c_str();  
        missingPars.push_back(name);
        }
    }else if (items[i]->iid() == Item::II_ASN){
       int index = missingPars.offset(items[i]->cast<AssignI>()->id().str());
       missingPars.erase(index);
    }
  }
    
  retVal.push_back(missingPars);  
  // return the string representation of the model
  stringstream strmodel;
  Printer *p = new Printer(strmodel); 
  p->print(model);
  CharacterVector mString;
  mString.push_back(strmodel.str());
  retVal.push_back(mString);
  retVal.names() = CharacterVector({"missingValues", "modelString"});
  return retVal;
}


