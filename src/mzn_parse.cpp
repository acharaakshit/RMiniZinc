#include <Rcpp.h>
#include <minizinc/parser.hh>
#include <minizinc/prettyprinter.hh>

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
//' @param mznfilename the name of model.
//' @param modelStringName the name of model string.
// [[Rcpp::export]]
List mzn_parse(std::string modelString = "", 
                      std::string mznfilename = "",
                      std::string  modelStringName = "abc.mzn"){
  // create a model and parse its items (make modifications to the model -- to be done)
  Model* model;
  if(modelString.empty() && mznfilename.empty()){
    Rcpp::stop("PROVIDE EITHER modelString OR mznfilename");
  }else{ 
    Env* env = new Env();
    vector<string> ip = {};
    ostringstream os;
    if(mznfilename.length()){
      //use parse
      std::vector<std::string> datafiles; 
      std::vector<std::string> filename;
      filename.push_back(mznfilename);
      try{
        model = MiniZinc::parse(*env, filename, datafiles, modelString, modelStringName,
                              ip, true, true, true, os);
        if(model==NULL) throw std::exception();
      }catch(std::exception& e){
        string parseError;
        parseError = os.str();
        Rcpp::stop(parseError);
      }
    }else{
      // use parsefromString
      vector<SyntaxError> se;
      try{
        model = MiniZinc::parseFromString(*env, modelString, modelStringName , ip, true, true, true, os, se);
        if(model==NULL) throw std::exception();
        else if(se.size()){
          Rcpp::stop(se[0].what());
        }
      }catch(std::exception& e){
        string parseError;
        parseError = os.str();
        Rcpp::stop(parseError);
      }
    }}
  
  List retVal;
  // size of the model
  int s = model-> size();
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


