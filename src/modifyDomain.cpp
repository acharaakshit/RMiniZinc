#include <Rcpp.h>
#include <fstream>
#include <minizinc/prettyprinter.hh>
#include "pathStringcheck.h"
#include "helper_parse.h"


using namespace std;
using namespace Rcpp;
using namespace MiniZinc;

//' @title update the variable domain
//' 
//' @desciption assign the new Id to min or max values of the domain
//' (Only for min..max operations).
//' @importFrom Rcpp sourceCpp
//' @export modifyDomainId
//' @useDynLib rminizinc, .registration=TRUE
//' @param ItemNo the item number of the variable whose domain is to be updated.
//' @param maxIdItem item number of the variable to be assigned as the max domain value.
//' @param minIdItem item number of the variable to be assigned as the min domain value.
//' @param replaceIdItem item number of the variable to be assigned as the new domain value.
//' @param modelString string representation of the MiniZinc model
//' @param mznpath path of the mzn file to read the model
//' @param modify_mzn if the user wants to modify the mzn parameters.
// [[Rcpp::export]]
std::string modifyDomainId(int ItemNo, int maxIdItem = -1, int minIdItem = -1,
                           int replaceIdItem = -1, std::string modelString = "",
                           std::string mznpath = "", bool modify_mzn = false) {
  
  modelString = pathStringcheck(modelString, mznpath);
  Model *model = helper_parse(modelString, "modifyDomainId.mzn");
  
  if(ItemNo >= model->size() || ItemNo < 0)  Rcpp::stop("item number is out of bounds");
  
  if(maxIdItem != -1) if(maxIdItem >= model->size() || maxIdItem < 0)  Rcpp::stop("maxItemId is out of bounds");
  if(minIdItem != -1) if(minIdItem >= model->size() || minIdItem < 0)  Rcpp::stop("minItemId is out of bounds");
  if(replaceIdItem != -1) if(replaceIdItem >= model->size() || replaceIdItem < 0)  Rcpp::stop("replaceItemId is out of bounds");
  
  Item *it = model->operator[](ItemNo);
  
  if(it->iid() == Item::II_VD){
    VarDecl *vd =  it->cast<VarDeclI>()->e();
    //Type mTp = vd->type();
    if(vd->ti()->domain() == NULL){
      Rcpp::stop("The specified Item doesn't have a domain");
    }else if(vd->ti()->domain()->eid() != Expression::E_BINOP && vd->ti()->domain()->eid() != Expression::E_SETLIT){
      Rcpp:stop("This function is only applicable to binary operator expressions or set literals like min..max");
    }
    else if(maxIdItem == -1 && minIdItem == -1 && replaceIdItem == -1){
      Rcpp::stop("Supply at least one of maxIdItem, minIdItem or replaceIdItem");
    }else if(maxIdItem == -1 && minIdItem != -1 && replaceIdItem == -1){
      Item *dit = model->operator[](minIdItem);
      if(dit->iid() != Item::II_VD) Rcpp::stop("minIdItem not a variable declaration");
      Type tp = dit->cast<VarDeclI>()->e()->type();
      //if(!tp.is_set()) Rcpp::warning("The supplied ItemId is not of a set variable");
      Expression *minLhs  = dit->cast<VarDeclI>()->e()->id();
      Expression *dExp = vd->ti()->domain();
      Expression *ndExp; 
      if(dExp->eid() == Expression::E_BINOP){
        BinOp *bo = dExp->cast<BinOp>(); 
        ndExp = new BinOp(it->loc(), minLhs, BinOpType::BOT_DOTDOT, bo->rhs()); 
      }else{
        SetLit *sl = dExp->cast<SetLit>();
        if(sl->isv() == NULL && sl->fsv()) Rcpp::stop("The set domain is not in a format of min..max");
        if(sl->isv() != NULL){
          ndExp = new BinOp(it->loc(), minLhs, BinOpType::BOT_DOTDOT, IntLit::a(sl->isv()->max().toInt()));  
        }else{
          ndExp = new BinOp(it->loc(), minLhs, BinOpType::BOT_DOTDOT, FloatLit::a(sl->fsv()->max().toDouble())); 
        }
      }
      vd->ti()->domain(ndExp);
      
    }else if(maxIdItem != -1 && minIdItem == -1 && replaceIdItem == -1){
      Item *dit = model->operator[](maxIdItem);
      if(dit->iid() != Item::II_VD) Rcpp::stop("maxIdItem not a variable declaration");
      Type tp = dit->cast<VarDeclI>()->e()->type();
      //if(!tp.is_set()) Rcpp::warning("The supplied ItemId is not of a set variable");
      Expression *maxRhs  = dit->cast<VarDeclI>()->e()->id();
      Expression *dExp = vd->ti()->domain();
      Expression *ndExp; 
      if(dExp->eid() == Expression::E_BINOP){
        BinOp *bo = dExp->cast<BinOp>(); 
        ndExp = new BinOp(it->loc(), bo->lhs() , BinOpType::BOT_DOTDOT, maxRhs); 
      }else{
        SetLit *sl = dExp->cast<SetLit>();
        if(sl->isv() == NULL && sl->fsv()) Rcpp::stop("The set domain is not in a format of min..max");
        if(sl->isv() != NULL){
          ndExp = new BinOp(it->loc(), IntLit::a(sl->isv()->min().toInt()), BinOpType::BOT_DOTDOT, maxRhs);  
        }else{
          ndExp = new BinOp(it->loc(), FloatLit::a(sl->fsv()->min().toDouble()), BinOpType::BOT_DOTDOT, maxRhs ); 
        }
      }
      vd->ti()->domain(ndExp);
    }else if(maxIdItem != -1 && minIdItem != -1 && replaceIdItem == -1){
      if(minIdItem == maxIdItem) Rcpp::warning("minIdItem and maxIdItem are equal");
      Item *dmaxIt = model->operator[](maxIdItem);
      Item *dminIt = model->operator[](minIdItem);
      if(dminIt->iid() != Item::II_VD) Rcpp::stop("minIdItem not a variable declaration");
      if(dmaxIt->iid() != Item::II_VD) Rcpp::stop("maxIdItem not a variable declaration");
      
      Type tpMin = dminIt->cast<VarDeclI>()->e()->type();
      Type tpMax = dmaxIt->cast<VarDeclI>()->e()->type();
      if(!tpMin.isint() || tpMin.isfloat()) Rcpp::stop("The supplied ItemId is not of int or float");
      if(!tpMax.isint() || tpMax.isfloat()) Rcpp::stop("The supplied ItemId is not of int or float");
      Expression *minRhs  = dmaxIt->cast<VarDeclI>()->e()->id();
      Expression *minLhs  = dminIt->cast<VarDeclI>()->e()->id();
      Expression *bo = new BinOp(it->loc(), minLhs, BinOpType::BOT_DOTDOT, minRhs); 
      vd->ti()->domain(bo);
    }else if(replaceIdItem != -1 && maxIdItem == -1 && minIdItem == -1){
      Item *dit = model->operator[](minIdItem);
      if(dit->iid() != Item::II_VD) Rcpp::stop("minIdItem not a variable declaration");
      Type tp = dit->cast<VarDeclI>()->e()->type();
      if(!tp.is_set()) Rcpp::warning("The supplied replaceItemId is not of a set variable");
      Expression *dExp  = dit->cast<VarDeclI>()->e()->id();
      vd->ti()->domain(dExp);
    }else {
      Rcpp::stop("Incorrect choice of parameters --  either replaceIdItem or either or both of minIdItem or maxIdItem can be supplied"); 
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


//' @title rcpp question
//' 
//' @desciption test if exceptions occur
//' @importFrom Rcpp sourceCpp
//' @export modifyDomainSetVal
//' @useDynLib rminizinc, .registration=TRUE
//' @param ItemNo the item number of the variable whose domain is to be updated.
//' @param imax maximum integer value of the int set variable domain.
//' @param imin minimum integer value of the int set variable domain.
//' @param fmin miminum float value of the float set variable domain.
//' @param fmax maximum float value of the float set variable domain.
//' @param modelString string representation of the MiniZinc model
//' @param mznpath path of the mzn file to read the model
//' @param modify_mzn if the user wants to modify the mzn parameters.
// [[Rcpp::export]]
std::string modifyDomainSetVal(int ItemNo, Nullable<int> imax = R_NilValue,
                               Nullable<int> imin = R_NilValue, Nullable<double> fmin = R_NilValue,
                             Nullable<double> fmax = R_NilValue, std::string modelString = "",
                             std::string mznpath = "", bool modify_mzn = false) {
  // do nothing
  modelString = pathStringcheck(modelString, mznpath);
  Model *model = helper_parse(modelString, "modifyDomainId.mzn");
  
  if(ItemNo >= model->size() || ItemNo < 0)  Rcpp::stop("item number is out of bounds");
  Item *it = model->operator[](ItemNo);
  if(it->iid() != Item::II_VD) Rcpp::stop("Given item number is not a variable declaration");
  VarDecl *vd =  it->cast<VarDeclI>()->e();
  
  if(vd->ti()->domain() == NULL){
    Rcpp::stop("The domain of the given item is NULL");
  }else if(vd->ti()->domain()->eid() != Expression::E_SETLIT){
    Rcpp::stop("The domain is not a set literal i.e it's not of the form minNum..maxNum");
  }else if(imin.isNull() && imax.isNull() && fmin.isNull() && fmax.isNull()) {
    Rcpp::stop("Please provide one of imin, imax, fmin or fmax");
  }else if(imin.isNull() && imax.isNotNull() && fmin.isNull() && fmax.isNull()){
    int nmax = Rcpp::as<int>(imax);
    Expression *dExp = vd->ti()->domain();
    SetLit *sl = dExp->cast<SetLit>();
    if(sl->isv() == NULL) Rcpp::stop("The given domain is not an integer set range");
    IntSetVal *isV = IntSetVal::a(sl->isv()->min(), IntVal(nmax));
    sl->isv(isV);
    Expression *slExp = sl;
    vd->ti()->domain(slExp);
  }else if(imin.isNotNull() && imax.isNull() && fmin.isNull() && fmax.isNull()){
    int nmin = Rcpp::as<int>(imin);
    Expression *dExp = vd->ti()->domain();
    SetLit *sl = dExp->cast<SetLit>();
    if(sl->isv() == NULL) Rcpp::stop("The given domain is not an integer set range");
    IntSetVal *isV = IntSetVal::a(IntVal(nmin), sl->isv()->max());
    sl->isv(isV);
    Expression *slExp = sl;
    vd->ti()->domain(slExp);
  }else if(imin.isNotNull() && imax.isNotNull() && fmin.isNull() && fmax.isNull()){
    int nmax = Rcpp::as<int>(imax);
    int nmin = Rcpp::as<int>(imin);
    Expression *dExp = vd->ti()->domain();
    SetLit *sl = dExp->cast<SetLit>();
    IntSetVal *isV = IntSetVal::a(IntVal(nmin), IntVal(nmax));
    sl->isv(isV);
    Expression *slExp = sl;
    vd->ti()->domain(slExp);
  }else if(imin.isNull() && imax.isNull() && fmin.isNull() && fmax.isNotNull()){
    double nmax = Rcpp::as<double>(fmax);
    Expression *dExp = vd->ti()->domain();
    SetLit *sl = dExp->cast<SetLit>();
    if(sl->fsv() == NULL) Rcpp::stop("The given domain is not a float set range");
    FloatSetVal *fsV = FloatSetVal::a(sl->fsv()->min(), FloatVal(nmax));
    sl->fsv(fsV);
    Expression *slExp = sl;
    vd->ti()->domain(slExp);
  }else if(imin.isNull() && imax.isNull() && fmin.isNotNull() && fmax.isNull()){
    double nmin = Rcpp::as<double>(fmin);
    Expression *dExp = vd->ti()->domain();
    SetLit *sl = dExp->cast<SetLit>();
    if(sl->fsv() == NULL) Rcpp::stop("The given domain is not a float set range");
    FloatSetVal *fsV = FloatSetVal::a(FloatVal(nmin), sl->fsv()->max());
    sl->fsv(fsV);
    Expression *slExp = sl;
    vd->ti()->domain(slExp);
  }else if(imin.isNull() && imax.isNull() && fmin.isNotNull() && fmax.isNotNull()){
    double nmax = Rcpp::as<double>(fmax);
    double nmin = Rcpp::as<double>(fmin);
    Expression *dExp = vd->ti()->domain();
    SetLit *sl = dExp->cast<SetLit>();
    FloatSetVal *fsV = FloatSetVal::a(FloatVal(nmin), FloatVal(nmax));
    sl->fsv(fsV);
    Expression *slExp = sl;
    vd->ti()->domain(slExp);
  }else{
    Rcpp::stop("Incorrect choice of parameters");
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

