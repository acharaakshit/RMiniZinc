#include <Rcpp.h>
#include "expVarNames.h"

using namespace Rcpp;
using namespace MiniZinc;
using namespace std;

void expVarNames(MiniZinc::Expression *exp, vector<string> &cstNames){
  int ExpressionID = exp->eid();
  if(ExpressionID == Expression::E_COMP){
    if(exp->cast<Comprehension>()->e()==NULL)
      Rcpp::stop("No comprehension expression found");
    int n_genrtrs =  exp->cast<Comprehension>()->n_generators();
    for(int i = 0; i<n_genrtrs;i++){
        Expression *inExp = exp->cast<Comprehension>()->in(i);
        Expression *whereExp = exp->cast<Comprehension>()->where(i);
        if(inExp != NULL){
          expVarNames(exp->cast<Comprehension>()->in(i), cstNames);  
        }else if(whereExp != NULL){
          expVarNames(exp->cast<Comprehension>()->where(i), cstNames); 
        }else{
          //no in or where expression found
        }
         
    }
    expVarNames(exp->cast<Comprehension>()->e(), cstNames);
  }else if(ExpressionID == Expression::E_CALL){
    int callArgs = exp->cast<Call>()->n_args();
    for(int i = 0;i< callArgs; i++){
      int itd =  exp->cast<Call>()->arg(i)->eid();
      expVarNames(exp->cast<Call>()->arg(i), cstNames);
    }
  }else if(ExpressionID == Expression::E_UNOP){
    UnOp *uo = exp->cast<UnOp>();
  }else if(ExpressionID == Expression::E_BINOP){
    BinOp *bo = exp->cast<BinOp>();
    expVarNames(bo->lhs(), cstNames);
    expVarNames(bo->rhs(), cstNames);
  }else if(ExpressionID == Expression::E_LET){
    Let *lt = exp->cast<Let>();
  }else if(ExpressionID == Expression::E_ID){
    string vName = exp->cast<Id>()->str().c_str();
    cstNames.push_back(vName);
  }else if(ExpressionID == Expression::E_INTLIT){
    // do nothing
  }else if(ExpressionID == Expression::E_ARRAYACCESS){
    expVarNames(exp->cast<ArrayAccess>()->v(), cstNames);
  }else{
    // cout << ExpressionID << " ID" << endl; -- will be supported later
  }
}