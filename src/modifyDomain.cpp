#include <Rcpp.h>
#include <fstream>
#include <minizinc/prettyprinter.hh>
#include "filetoString.h"
#include "helper_parse.h"


using namespace std;
using namespace Rcpp;
using namespace MiniZinc;

//' @title update the variable domain
//' 
//' @desciption assign the new domain of the variable.
//' 
//' @importFrom Rcpp sourceCpp
//' @export modifyDomain
//' @useDynLib rminizinc, .registration=TRUE
//' @param ItemNo the item number of the variable whose domain is to be updated.
//' @param NumericSetVal list containing the numeric set values. (should be strcitly
//' of the form c(max = r_value, min = l_value))
//' @param IdItem the name of the set variable to be assigned as the set value.
//' @param modelString string representation of the MiniZinc model
//' @param mznpath path of the mzn file to read the model
//' @param modify_mzn if the user wants to modify the mzn parameters.
// [[Rcpp::export]]
std::string modifyDomain(int ItemNo, Rcpp::Nullable<Rcpp::NumericVector> NumericSetVal = R_NilValue,
                         int IdItem = -1, std::string modelString = "", std::string mznpath = "",
                       bool modify_mzn = false) {
  
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
  
  Model *model = helper_parse(modelString, "modifyDom.mzn");
  
  if(ItemNo >= model->size() || ItemNo < 0)  Rcpp::stop("item number is out of bounds");
  
  if(IdItem != -1) if(IdItem >= model->size() || IdItem < 0)  Rcpp::stop("itemId is out of bounds");
  
  Item *it = model->operator[](ItemNo);
  if(it->iid() == Item::II_VD){
    VarDecl *vd =  it->cast<VarDeclI>()->e();
    if(Rf_isNull(NumericSetVal) && IdItem == -1){
      Rcpp::stop("Supply the new value of the domain to be set");
    }else if(!Rf_isNull(NumericSetVal) && IdItem == -1){
      CharacterVector cmpWith = CharacterVector({"max", "min"});
      NumericVector sV(NumericSetVal);
      CharacterVector nsNames = sV.names();
      std::equal(nsNames.begin(), nsNames.end(), cmpWith.begin());
      Expression *lhs;
      Expression *rhs;
      if(vd->type().bt() == Type::BT_INT){
        lhs = IntLit::a((int)sV["min"]);
        rhs = IntLit::a((int)sV["max"]);  
      }else if(vd->type().bt() == Type::BT_FLOAT ){
        lhs = IntLit::a((double)sV["min"]);
        rhs = IntLit::a((double)sV["max"]);
      }else if(vd->type().bt() == Type::BT_UNKNOWN && !vd->type().is_set() && vd->type().dim() >=1){
        Rcpp::warning("The values of domain will be coerced into integers");
        lhs = IntLit::a((int)sV["min"]);
        rhs = IntLit::a((int)sV["max"]);  
      }else{
        Rcpp::stop("Can't set domain -- base type is not int or float");
      }
      Expression *xBo = new BinOp(it->loc() ,lhs, BinOpType::BOT_DOTDOT, rhs); 
      vd->ti()->domain(xBo);
    }else if(Rf_isNull(NumericSetVal) && IdItem != -1){
      Item *dit = model->operator[](IdItem);
      if(dit->iid() != Item::II_VD) Rcpp::stop("ItemId not a variable declaration");
      Type tp = dit->cast<VarDeclI>()->e()->type();
      if(!tp.is_set()) Rcpp::warning("The supplied ItemId is not of a set variable");
      Expression *dExp  = dit->cast<VarDeclI>()->e()->id();
      vd->ti()->domain(dExp);
    }else{
      Rcpp::stop("Please supply only one of NumericSetVal and ItemId");
    } 
  }else{
    Rcpp::stop("item number is not of a variable declaration");
  }
  
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