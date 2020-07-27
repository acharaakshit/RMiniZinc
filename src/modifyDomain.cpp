#include <Rcpp.h>
#include <fstream>
#include <minizinc/prettyprinter.hh>
#include "pathStringcheck.h"
#include "helper_parse.h"


using namespace std;
using namespace Rcpp;
using namespace MiniZinc;

// Helper function
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
  if(it->iid() != Item::II_VD) Rcpp::stop("Given item number is not a variable declaration");
  VarDecl *vd =  it->cast<VarDeclI>()->e();
  Expression *dExp = vd->ti()->domain();
  
    if(dExp == NULL){
      Rcpp::stop("The specified Item doesn't have a domain");
    }else if(dExp->eid() != Expression::E_BINOP && dExp->eid() != Expression::E_SETLIT){
      Rcpp:stop("This function is only applicable to binary operator expressions or set literals like min..max");
    }else if(maxIdItem == -1 && minIdItem == -1 && replaceIdItem == -1){
      Rcpp::stop("Supply at least one of maxIdItem, minIdItem or replaceIdItem");
    }else if(maxIdItem == -1 && minIdItem != -1 && replaceIdItem == -1){
      Item *dit = model->operator[](minIdItem);
      if(dit->iid() != Item::II_VD) Rcpp::stop("minIdItem not a variable declaration");
      Type tp = dit->cast<VarDeclI>()->e()->type();
      Expression *minLhs  = dit->cast<VarDeclI>()->e()->id();
      Expression *ndExp; 
      if(dExp->eid() == Expression::E_BINOP){
        BinOp *bo = dExp->cast<BinOp>(); 
        if(bo->op() != BinOpType::BOT_DOTDOT) Rcpp::stop("Not a range binary expression");
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
      if(!tp.is_set()) Rcpp::warning("The supplied ItemId is not of a set variable");
      Expression *maxRhs  = dit->cast<VarDeclI>()->e()->id();
      Expression *ndExp; 
      if(dExp->eid() == Expression::E_BINOP){
        BinOp *bo = dExp->cast<BinOp>(); 
        if(bo->op() != BinOpType::BOT_DOTDOT) Rcpp::stop("Not a range binary expression");
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
    
    return getnWriteStr(mznpath, model, modify_mzn);
}


//' @title Modify domain using integer or floating point values 
//' 
//' @desciption Assign integer or float values to domains which are of type min..max
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
  
  modelString = pathStringcheck(modelString, mznpath);
  Model *model = helper_parse(modelString, "modifyDomainId.mzn");
  
  if(ItemNo >= model->size() || ItemNo < 0)  Rcpp::stop("item number is out of bounds");
  Item *it = model->operator[](ItemNo);
  if(it->iid() != Item::II_VD) Rcpp::stop("Given item number is not a variable declaration");
  VarDecl *vd =  it->cast<VarDeclI>()->e();
  Expression *dExp = vd->ti()->domain();
  
  if(dExp == NULL){
    Rcpp::stop("The domain of the given item is NULL");
  }else if(dExp->eid() != Expression::E_SETLIT && dExp->eid() != Expression::E_BINOP ){
    Rcpp::stop("The domain is not a set literal i.e it's not of the form minNum..maxNum");
  }else if(imin.isNull() && imax.isNull() && fmin.isNull() && fmax.isNull()) {
    Rcpp::stop("Please provide one of imin, imax, fmin or fmax");
  }else if(imin.isNull() && imax.isNotNull() && fmin.isNull() && fmax.isNull()){
    int nmax = Rcpp::as<int>(imax);
    if(dExp->eid() == Expression::E_SETLIT){
      SetLit *sl = dExp->cast<SetLit>();
      if(sl->isv() == NULL) Rcpp::stop("The given domain is not an integer set range");
      IntSetVal *isV = IntSetVal::a(sl->isv()->min(), IntVal(nmax));
      sl->isv(isV);
      Expression *slExp = sl; 
      vd->ti()->domain(slExp);
    }else{
      BinOp *bo = dExp->cast<BinOp>();
      if(bo->op() != BinOpType::BOT_DOTDOT) Rcpp::stop("Not a range binary expression");
      Expression *nbo = new BinOp(it->loc(), bo->lhs(), bo->op(), IntLit::a(nmax));
      vd->ti()->domain(nbo);
    }
  }else if(imin.isNotNull() && imax.isNull() && fmin.isNull() && fmax.isNull()){
    int nmin = Rcpp::as<int>(imin);
    if(dExp->eid() == Expression::E_SETLIT){
      SetLit *sl = dExp->cast<SetLit>();
      if(sl->isv() == NULL) Rcpp::stop("The given domain is not an integer set range");
      IntSetVal *isV = IntSetVal::a(IntVal(nmin), sl->isv()->max());
      sl->isv(isV);
      Expression *slExp = sl;
      vd->ti()->domain(slExp); 
    }else{
      BinOp *bo = dExp->cast<BinOp>();
      if(bo->op() != BinOpType::BOT_DOTDOT) Rcpp::stop("Not a range binary expression");
      Expression *nbo = new BinOp(it->loc(),  IntLit::a(nmin), bo->op(), bo->rhs());
      vd->ti()->domain(nbo);
    }
  }else if(imin.isNotNull() && imax.isNotNull() && fmin.isNull() && fmax.isNull()){
    int nmax = Rcpp::as<int>(imax);
    int nmin = Rcpp::as<int>(imin);
    if(dExp->eid() == Expression::E_SETLIT){
      SetLit *sl = dExp->cast<SetLit>();
      IntSetVal *isV = IntSetVal::a(IntVal(nmin), IntVal(nmax));
      sl->isv(isV);
      Expression *slExp = sl;
      vd->ti()->domain(slExp); 
    }else{
      BinOp *bo = dExp->cast<BinOp>();
      if(bo->op() != BinOpType::BOT_DOTDOT) Rcpp::stop("Not a range (..) binary expression");
      Expression *nbo = new BinOp(it->loc(),  IntLit::a(nmin), bo->op(), IntLit::a(nmax));
      vd->ti()->domain(nbo);
    }
  }else if(imin.isNull() && imax.isNull() && fmin.isNull() && fmax.isNotNull()){
    double nmax = Rcpp::as<double>(fmax);
    if(dExp->eid() == Expression::E_SETLIT){
      SetLit *sl = dExp->cast<SetLit>();
      if(sl->fsv() == NULL) Rcpp::stop("The given domain is not a float set range");
      FloatSetVal *fsV = FloatSetVal::a(sl->fsv()->min(), FloatVal(nmax));
      sl->fsv(fsV);
      Expression *slExp = sl;
      vd->ti()->domain(slExp);
    }else{
      BinOp *bo = dExp->cast<BinOp>();
      if(bo->op() != BinOpType::BOT_DOTDOT) Rcpp::stop("Not a range binary expression");
      Expression *nbo = new BinOp(it->loc(), bo->lhs(), bo->op(), FloatLit::a(nmax));
      vd->ti()->domain(nbo);
    }
  }else if(imin.isNull() && imax.isNull() && fmin.isNotNull() && fmax.isNull()){
    double nmin = Rcpp::as<double>(fmin);
    if(dExp->eid() == Expression::E_SETLIT){
      SetLit *sl = dExp->cast<SetLit>();
      if(sl->fsv() == NULL) Rcpp::stop("The given domain is not a float set range");
      FloatSetVal *fsV = FloatSetVal::a(FloatVal(nmin), sl->fsv()->max());
      sl->fsv(fsV);
      Expression *slExp = sl;
      vd->ti()->domain(slExp);
    }else{
      BinOp *bo = dExp->cast<BinOp>();
      if(bo->op() != BinOpType::BOT_DOTDOT) Rcpp::stop("Not a range binary expression");
      Expression *nbo = new BinOp(it->loc(),  FloatLit::a(nmin), bo->op(), bo->rhs());
      vd->ti()->domain(nbo);
    }
  }else if(imin.isNull() && imax.isNull() && fmin.isNotNull() && fmax.isNotNull()){
    double nmax = Rcpp::as<double>(fmax);
    double nmin = Rcpp::as<double>(fmin);
    if(dExp->eid() == Expression::E_SETLIT){
      SetLit *sl = dExp->cast<SetLit>();
      FloatSetVal *fsV = FloatSetVal::a(FloatVal(nmin), FloatVal(nmax));
      sl->fsv(fsV);
      Expression *slExp = sl;
      vd->ti()->domain(slExp); 
    }else{
      BinOp *bo = dExp->cast<BinOp>();
      if(bo->op() != BinOpType::BOT_DOTDOT) Rcpp::stop("Not a range (..) binary expression");
      Expression *nbo = new BinOp(it->loc(),  FloatLit::a(nmin), bo->op(), FloatLit::a(nmax));
      vd->ti()->domain(nbo);
    }
  }else{
    Rcpp::stop("Incorrect choice of parameters");
  }
  
  return getnWriteStr(mznpath, model, modify_mzn);
}


//' @title Modify domain function calls
//' 
//' @desciption Assign max(Id) or min(Id) function calls to domains.
//' @importFrom Rcpp sourceCpp
//' @export modifyDomainFnCall
//' @useDynLib rminizinc, .registration=TRUE
//' @param ItemNo the item number of the variable whose domain is to be updated.
//' @param minFnName Name of the function to be used for the min ID in min..max.
//' @param maxFnName Name of the function to be used for the max ID in min..max.
//' @param minIdItem min Id item number in domains of type min..max.
//' @param maxIdItem max Id item number in domains of type min..max.
//' @param modelString string representation of the MiniZinc model
//' @param mznpath path of the mzn file to read the model
//' @param modify_mzn if the user wants to modify the mzn parameters.
// [[Rcpp::export]]
std::string modifyDomainFnCall(int ItemNo, int minIdItem = -1,
                                int maxIdItem = -1, 
                                std::string minFnName = "",
                                std::string maxFnName = "",
                                std::string modelString = "",
                                std::string mznpath = "", bool modify_mzn = false){
  
  modelString = pathStringcheck(modelString, mznpath);
  Model *model = helper_parse(modelString, "modifyDomainId.mzn");
  
  if(minFnName.compare("max") != 0 && minFnName.compare("min") != 0 && !minFnName.empty())
   Rcpp::stop("Function not supported yet!");
  
  if(maxFnName.compare("max") != 0 && maxFnName.compare("min") != 0 && !maxFnName.empty())
      Rcpp::stop("Function not supported yet!");
  
  if(ItemNo >= model->size() || ItemNo < 0)  Rcpp::stop("item number is out of bounds");
  Item *it = model->operator[](ItemNo);
  
  if(maxIdItem != -1) if(maxIdItem >= model->size() || maxIdItem < 0)  Rcpp::stop("maxItemId is out of bounds");
  if(minIdItem != -1) if(minIdItem >= model->size() || minIdItem < 0)  Rcpp::stop("minItemId is out of bounds");
  
  if(it->iid() != Item::II_VD) Rcpp::stop("Given item number is not a variable declaration");
  VarDecl *vd =  it->cast<VarDeclI>()->e();
  Expression *dExp = vd->ti()->domain();
  
  if(dExp == NULL){
    Rcpp::stop("The domain of the given item is NULL");
  }else if(dExp->eid() != Expression::E_BINOP && dExp->eid() != Expression::E_SETLIT){
    Rcpp::stop("The domain is not a binary ..  operation or set range");
  }else if(minIdItem == -1 && maxIdItem == -1) {
    Rcpp::stop("Please provide one of minIdItem or maxIdItem");
  }else if(minIdItem != -1 && maxIdItem == -1){
    if(minFnName.empty()) Rcpp::stop("Please provide the function to be used");
    Item *minItem = model->operator[](minIdItem);
    if(minItem->iid() != Item::II_VD)
      Rcpp::stop("The minIdItem is not a variable declaration");
    Expression *idExp = minItem->cast<VarDeclI>()->e()->id();
    vector<Expression*> fnExp ={idExp};
    try{
      Expression *clExp = new Call(it->loc(), 
                                   minFnName, fnExp);
      if(dExp->eid() == Expression::E_BINOP){
        BinOp *biExp = dExp->cast<BinOp>();
        if(biExp->op() != BinOpType::BOT_DOTDOT) Rcpp::stop("Not a range binary expression");
        Expression *nbiExp = new BinOp(it->loc(), clExp, BinOpType::BOT_DOTDOT, biExp->rhs());
        vd->ti()->domain(nbiExp);
      }else{
        SetLit *sl = dExp->cast<SetLit>();
        Expression *nbiExp;
        if(sl->isv() != NULL)
          nbiExp = new BinOp(it->loc(), clExp, BinOpType::BOT_DOTDOT, IntLit::a(sl->isv()->max().toInt()));
        else if(sl->fsv() != NULL)
          nbiExp = new BinOp(it->loc(), clExp, BinOpType::BOT_DOTDOT, FloatLit::a(sl->fsv()->max().toDouble()));
        else 
          Rcpp::stop("Only set ranges are supported!");
        vd->ti()->domain(nbiExp);
      }
    }catch(MiniZinc::Exception &e){
      Rcpp::stop(e.what());
    }catch(...){
      Rcpp::stop("unknown exception!");
    }
  }else if(minIdItem == -1 && maxIdItem != -1){
    if(maxFnName.empty()) Rcpp::stop("Please provide the function to be used");
    Item *maxItem = model->operator[](maxIdItem);
    if(maxItem->iid() != Item::II_VD)
      Rcpp::stop("The minIdItem is not a variable declaration");
    Expression *idExp = maxItem->cast<VarDeclI>()->e()->id();
    vector<Expression*> fnExp = {idExp};
    try{
      Expression *clExp = new Call(it->loc(), 
                                   maxFnName, fnExp);
      if(dExp->eid() == Expression::E_BINOP){
        BinOp *biExp = dExp->cast<BinOp>();
        if(biExp->op() != BinOpType::BOT_DOTDOT) Rcpp::stop("Not a range binary expression");
        Expression *nbiExp = new BinOp(it->loc(), biExp->lhs(), BinOpType::BOT_DOTDOT, clExp);
        vd->ti()->domain(nbiExp);
      }else{
        SetLit *sl = dExp->cast<SetLit>();
        Expression *nbiExp;
        if(sl->isv() != NULL)
          nbiExp = new BinOp(it->loc(), IntLit::a(sl->isv()->max().toInt()), BinOpType::BOT_DOTDOT, clExp);
        else if(sl->fsv() != NULL)
          nbiExp = new BinOp(it->loc(), FloatLit::a(sl->fsv()->max().toDouble()), BinOpType::BOT_DOTDOT, clExp);
        else 
          Rcpp::stop("Only set ranges are supported!");
        vd->ti()->domain(nbiExp);
      }
    }catch(MiniZinc::Exception &e){
      Rcpp::stop(e.what());
    }catch(...){
      Rcpp::stop("unknown exception!");
    }
  }else{
    if(minFnName.empty()) Rcpp::stop("Please provide the function to be used");
    Item *minItem = model->operator[](minIdItem);
    Rcpp::stop("Not supported currently");
    if(minItem->iid() != Item::II_VD)
      Rcpp::stop("The minIdItem is not a variable declaration");
    if(maxFnName.empty()) Rcpp::stop("Please provide the function to be used");
    Item *maxItem = model->operator[](maxIdItem);
    if(maxItem->iid() != Item::II_VD)
      Rcpp::stop("The minIdItem is not a variable declaration");
    Expression *minidExp = minItem->cast<VarDeclI>()->e()->id();
    Expression *maxidExp = maxItem->cast<VarDeclI>()->e()->id();
    vector<Expression*> fnminExp = {minidExp};
    vector<Expression*> fnmaxExp = {maxidExp};
    try{
      Expression *clminExp = new Call(it->loc(), 
                                      minFnName, fnminExp);
      Expression *clmaxExp = new Call(it->loc(), 
                                   maxFnName, fnmaxExp);
      Expression *nbiExp = new BinOp(it->loc(), clminExp, BinOpType::BOT_DOTDOT, clmaxExp);
      vd->ti()->domain(nbiExp);
    }catch(MiniZinc::Exception &e){
      Rcpp::stop(e.what());
    }catch(...){
      Rcpp::stop("unknown exception!");
    }
  }
  return getnWriteStr(mznpath, model, modify_mzn);
}

// Helper function
MiniZinc::BinOpType getOP(std::string OP){
  if(OP.compare("PLUS") == 0 ) return BinOpType::BOT_PLUS;
  else if(OP.compare("MINUS") == 0) return BinOpType::BOT_MINUS;
  else if(OP.compare("MULT") == 0) return BinOpType::BOT_MULT;
  else if(OP.compare("MOD") == 0) return  BinOpType::BOT_MOD;
  else if(OP.compare("DIV") == 0) return BinOpType::BOT_DIV;
  else Rcpp::stop("unrecognised operator");
}


//' @title Modify domain using constants
//' 
//' @desciption Add, subtract, multiply or divide constants from domain expressions.
//' @importFrom Rcpp sourceCpp
//' @export modifyDomainAO
//' @useDynLib rminizinc, .registration=TRUE
//' @param ItemNo the item number of the variable whose domain is to be updated.
//' @param minVal value to add/subtract/multiply/divide to min in domains of type min..max.
//' @param maxVal value to add/subtract/multiply/divide to max in domains of type min..max.
//' @param Val value to add/subract/multiply/divide to any expression of domain.
//' @param OPmin string which specifies the operator to be used with minVal.(PLUS, MINUS,MOD,DIV,MULT) 
//' @param OPmax string which specifies the operator to be used with maxVal.(PLUS, MINUS,MOD,DIV,MULT) 
//' @param OP string which specifies the operator to be used with Val.(PLUS, MINUS,MOD,DIV,MULT) 
//' @param modelString string representation of the MiniZinc model
//' @param mznpath path of the mzn file to read the model
//' @param modify_mzn if the user wants to modify the mzn parameters.
// [[Rcpp::export]]
std:: string modifyDomainAO(int ItemNo, SEXP minVal = R_NilValue, 
                            SEXP maxVal = R_NilValue, SEXP Val = R_NilValue, 
                            std::string OPmin = "", std::string OPmax = "",
                            std::string OP = "",
                            std::string modelString = "",
                            std::string mznpath = "", bool modify_mzn = false){
  modelString = pathStringcheck(modelString, mznpath);
  Model *model = helper_parse(modelString, "modifyDomainId.mzn");
  
  if(ItemNo >= model->size() || ItemNo < 0)  Rcpp::stop("item number is out of bounds");
  
  Item *it = model->operator[](ItemNo);
  
  BinOpType op;
  
  if(it->iid() != Item::II_VD) Rcpp::stop("Item is not a variable declaration");
  
    VarDecl *vd =  it->cast<VarDeclI>()->e();
    Type itp = it->cast<VarDeclI>()->e()->type();
    Expression *dExp = vd->ti()->domain();
    if(dExp == NULL){
      Rcpp::stop("The specified Item doesn't have a domain");
    }else if(!Rf_isNull(minVal) && Rf_isNull(maxVal) && Rf_isNull(Val)){
      if(OPmin.empty()) Rcpp::stop("Please provide the arithmetic operator");
      op = getOP(OPmin);
      Expression *dExp = vd->ti()->domain();
      if(itp.bt() == Type::BT_FLOAT){
        double imin = Rcpp::as<double>(minVal);
        if(dExp->eid() != Expression::E_BINOP) Rcpp::stop("domain is not a .. binary operator");
        BinOp *bo = dExp->cast<BinOp>();
        Expression  *nlhs = new BinOp(it->loc(), bo->lhs(), op, FloatLit::a(imin));
        bo->lhs(nlhs);
        Expression*ndExp = bo;
        vd->ti()->domain(ndExp);
      }else if (itp.bt() == Type::BT_INT || itp.bt() == Type::BT_UNKNOWN){
        if(itp.bt() == Type::BT_UNKNOWN) Rcpp::warning("The data type of the variable is not mentioned, casting to int.");
        int imin = Rcpp::as<int>(minVal);
        if(dExp->eid() != Expression::E_BINOP) Rcpp::stop("domain is not a .. binary operator");
        BinOp *bo = dExp->cast<BinOp>();
        Expression  *nlhs = new BinOp(it->loc(), bo->lhs(), op, IntLit::a(imin));
        bo->lhs(nlhs);
        Expression*ndExp = bo;
        vd->ti()->domain(ndExp);
      }else{
        Rcpp::stop("Unrecognized Variable Type");
      }      
    }else if(Rf_isNull(minVal) && !Rf_isNull(maxVal) && Rf_isNull(Val)){
      if(OPmax.empty()) Rcpp::stop("Please provide the arithmetic operator");
      op = getOP(OPmax);
      Expression *dExp = vd->ti()->domain();
      if(itp.bt() == Type::BT_FLOAT){
        double imax = Rcpp::as<double>(maxVal);
        if(dExp->eid() != Expression::E_BINOP) Rcpp::stop("domain is not a .. binary operator");
        BinOp *bo = dExp->cast<BinOp>();
        Expression  *nrhs = new BinOp(it->loc(), bo->rhs(), op, FloatLit::a(imax));
        bo->rhs(nrhs);
        Expression*ndExp = bo;
        vd->ti()->domain(ndExp);
      }else if (itp.bt() == Type::BT_INT || itp.bt() == Type::BT_UNKNOWN){
        if(itp.bt() == Type::BT_UNKNOWN) Rcpp::warning("The data type of the variable is not mentioned, casting to int.");
        int imax = Rcpp::as<int>(maxVal);
        if(dExp->eid() != Expression::E_BINOP) Rcpp::stop("domain is not a .. binary operator");
        BinOp *bo = dExp->cast<BinOp>();
        Expression  *nrhs = new BinOp(it->loc(), bo->rhs(), op, IntLit::a(imax));
        bo->rhs(nrhs);
        Expression*ndExp = bo;
        vd->ti()->domain(ndExp);
      }else{
        Rcpp::stop("Unrecognized Variable Type");
      } 
    }else if(!Rf_isNull(minVal) && !Rf_isNull(maxVal) && Rf_isNull(Val)){
      if(OPmax.empty()) Rcpp::stop("Please provide the arithmetic operator");
      if(OPmin.empty()) Rcpp::stop("Please provide the arithmetic operator");
      BinOpType opMin = getOP(OPmin);
      BinOpType opMax = getOP(OPmax);
      Expression *dExp = vd->ti()->domain();
      if(itp.bt() == Type::BT_FLOAT){
        double imax = Rcpp::as<double>(maxVal);
        double imin = Rcpp::as<double>(minVal);
        if(dExp->eid() != Expression::E_BINOP) Rcpp::stop("domain is not a .. binary operator");
        BinOp *bo = dExp->cast<BinOp>();
        Expression  *nlhs = new BinOp(it->loc(), bo->lhs(), opMin, FloatLit::a(imin));
        Expression *nrhs = new BinOp(it->loc(), bo->rhs(), opMax, FloatLit::a(imax));
        bo->lhs(nlhs);
        bo->rhs(nrhs);
        Expression*ndExp = bo;
        vd->ti()->domain(ndExp);
      }else if (itp.bt() == Type::BT_INT || itp.bt() == Type::BT_UNKNOWN){
        if(itp.bt() == Type::BT_UNKNOWN) Rcpp::warning("The data type of the variable is not mentioned, casting to int.");
        int imax = Rcpp::as<int>(maxVal);
        int imin = Rcpp::as<int>(minVal);
        if(dExp->eid() != Expression::E_BINOP) Rcpp::stop("domain is not a .. binary operator");
        BinOp *bo = dExp->cast<BinOp>();
        Expression  *nlhs = new BinOp(it->loc(), bo->lhs(), opMin, IntLit::a(imin));
        Expression *nrhs = new BinOp(it->loc(), bo->rhs(), opMax, IntLit::a(imax));
        bo->lhs(nlhs);
        bo->rhs(nrhs);
        Expression*ndExp = bo;
        vd->ti()->domain(ndExp);
      }else{
        Rcpp::stop("Unrecognized Variable Type");
      } 
    }else if(Rf_isNull(minVal) && Rf_isNull(maxVal) && !Rf_isNull(Val)){
      if(OP.empty()) Rcpp::stop("Please provide the arithmetic operator");
      op = getOP(OP);
      Expression *dExp = vd->ti()->domain();
      if(itp.bt() == Type::BT_FLOAT){
        double val = Rcpp::as<double>(Val);
        Expression  *ndExp = new BinOp(it->loc(), dExp, op, FloatLit::a(val));
        vd->ti()->domain(ndExp);
      }else if (itp.bt() == Type::BT_INT || itp.bt() == Type::BT_UNKNOWN){
        if(itp.bt() == Type::BT_UNKNOWN) Rcpp::warning("The data type of the variable is not mentioned, casting to int.");
        int val = Rcpp::as<int>(Val);
        Expression  *ndExp = new BinOp(it->loc(), dExp, op, IntLit::a(val));
        vd->ti()->domain(ndExp);
      }else{
        Rcpp::stop("Unrecognized Variable Type");
      } 
    }else{
      Rcpp::stop("Incorrect choice of parameters");
    }     

    return getnWriteStr(mznpath, model, modify_mzn);
}
