#include <Rcpp.h>
#include "helper_sol_parse.h"


using namespace Rcpp;
using namespace MiniZinc;
using namespace std;

void helper_sol_parse(MiniZinc::Expression *sExp,  Rcpp::List &thisSol){
  
  int dataType = sExp->eid();
  if(dataType==Expression::E_INTLIT){
    thisSol.push_back(sExp->cast<IntLit>()->v().toInt());
  }else if(dataType==Expression::E_FLOATLIT){
    thisSol.push_back(sExp->cast<FloatLit>()->v().toDouble()); 
  }else if(dataType==Expression::E_BOOLLIT){
    thisSol.push_back(sExp->cast<BoolLit>()->v()); 
  }else if(dataType==Expression::E_SETLIT){
    SetLit *sl = sExp->cast<SetLit>();
    if(sl->isv()!= NULL){  
      int max_val = sl->isv()->max().toInt();
      int min_val = sl->isv()->min().toInt();  
      IntegerVector setVec = {max_val, min_val}; 
      setVec.names() = CharacterVector({"max", "min"});
      thisSol.push_back(setVec);
    }else if(sl->fsv()!=NULL){
      float max_val =  sl->fsv()->max().toDouble();
      float min_val =  sl->fsv()->min().toDouble();
      NumericVector setVec = {max_val, min_val};
      setVec.names() = CharacterVector({"max", "min"});
      thisSol.push_back(setVec);
    }else{
      ASTExprVec<Expression> expVec = sl->v();
      int expVec_size = expVec.size();
      List setVec;
      for(int p = 0; p < expVec_size; p++){
        Expression *exp = expVec.operator[](p);
        helper_sol_parse(exp, setVec);
      }
      thisSol.push_back(setVec);
    } 
  }else if(dataType == Expression::E_ARRAYLIT){
    ArrayLit *al = sExp->cast<ArrayLit>();
    if(al->getVec().size()){
      int vec_size = al->getVec().size();
      List ArrVec;
      for(int p = 0;p < vec_size; p++ ){
        // get the expression form of each element
        Expression *exp = al->getVec().operator[](p);
        helper_sol_parse(exp, ArrVec);
      }
      thisSol.push_back(ArrVec);
    }   
  }else if(dataType == Expression::E_CALL){
    Call *cl = sExp->cast<Call>();
    // name of the function
    string fnName = cl->id().c_str();
    NumericVector dimVec;
    for(int m = 0;m<cl->n_args();m++){
      int fntype = cl->arg(m)->eid();
      // apply switch case again 
      if(fntype==Expression::E_ARRAYLIT){
        //number of elements in the array
        int vec_size = cl->arg(m)->cast<ArrayLit>()->getVec().size();
        List ArrVec;
        for(int p = 0;p < vec_size; p++ ){
          // get the expression form of each element
          Expression *exp = cl->arg(m)->cast<ArrayLit>()->getVec().operator[](p);
          helper_sol_parse(exp, ArrVec);
        }
        if(dimVec.length()==2){
          if(dimVec[0]*dimVec[1] != vec_size){
            Rcpp:stop("Incorrect dimensions for array2d");
          } 
          ArrVec.attr("dim") = Dimension(dimVec[0], dimVec[1]);
        }else if(dimVec.length()>2){
          Rcpp::stop("More than 2d arrays not supported yet!");
        }
        thisSol.push_back(ArrVec);
      }else if(fntype == Expression::E_SETLIT){
        SetLit *sl = cl->arg(m)->cast<SetLit>();
        if(sl->isv()!= NULL){  
          int max_val =  sl->isv()->max().toInt();
          int min_val = sl->isv()->min().toInt();  
          // dimensions will be stored as 0 to max_val-min_val+1
          dimVec.push_back(max_val-min_val+1);
        }else{
          Rcpp::stop("only integer set range allowed for arrays"); 
        }
      }else{
        Rcpp::stop("function call not supported");
      }
    }}
  else{
    Rcpp::stop("not supported");
  }
      
}
