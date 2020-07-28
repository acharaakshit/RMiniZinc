#include <Rcpp.h>
#include "helper_modify.h"


using namespace Rcpp;
using namespace std;
using namespace MiniZinc;
  
MiniZinc::Item* DomainItemCheck(MiniZinc::Model *model, int ItemNo){
  if(ItemNo >= model->size() || ItemNo < 0)  Rcpp::stop("item number is out of bounds");
  Item *it = model->operator[](ItemNo);
  if(it->iid() != Item::II_VD) Rcpp::stop("Given item number is not a variable declaration");
  return it;
}

std::string getnWriteStr(std::string mznpath, MiniZinc::Model* model, bool modify_mzn){
  
  stringstream strmodel;
  Printer *p = new Printer(strmodel); 
  p->print(model);
  string mString = strmodel.str();
  
  if(!mznpath.empty() && modify_mzn){
    ofstream out(mznpath);
    out << mString;
    out.close();
  }else if(modify_mzn){
    Rcpp::warning("no file given to modify");
  }
  return mString;
}