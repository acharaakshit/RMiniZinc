#include <Rcpp.h>
#include <minizinc/ast.hh>
#include <minizinc/prettyprinter.hh>
#include <minizinc/exception.hh>
#include "filetoString.h"
#include "helper_parse.h"

using namespace std;
using namespace MiniZinc;

using namespace Rcpp;

//' @title Delete an item from a MiniZinc model
//' 
//' @description deletes an item from MiniZinc based on its item
//' number that is obtained from mzn_parse() function.
//'
//' @importFrom Rcpp sourceCpp
//' @export deleteItem
//' @useDynLib rminizinc, .registration=TRUE
//' @param itemNo the item number of the item to be deleted.
//' @param modelString string representation of the MiniZinc model.
//' @param mznpath the path of model mzn.
//' @param modelStringName the name of model string.
//' @param updateMZN bool to specify if the new model should be written to given file.
// [[Rcpp::export]]
std::string deleteItem(int itemNo,
                std::string modelString = "", 
                std::string mznpath = "",
                std::string  modelStringName = "deleteItem.mzn",
                bool updateMZN = false){
  
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
  
  if(itemNo >= model->size() || itemNo < 0)  Rcpp::stop("item number is out of bounds");
  
  // delete the item
  Item *it = model->operator[](itemNo);
  it->remove();
  model->compact();
  
  string retVal;
  stringstream strmodel;
  Printer *p = new Printer(strmodel); 
  p->print(model);
  retVal = strmodel.str();
  
  if(!mznpath.empty() && updateMZN == true){
    ofstream out(mznpath);
    out << retVal;
    out.close();
  }else if(updateMZN){
    Rcpp::warning("no file given to modify");
  }
  return retVal;
}