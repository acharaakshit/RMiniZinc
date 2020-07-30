#include <Rcpp.h>
#include "helper_sol_parse.h"


using namespace Rcpp;
using namespace MiniZinc;
using namespace std;

void helper_sol_parse(MiniZinc::Expression *sExp,  Rcpp::List &thisSol){
  if(sExp->eid()==Expression::E_INTLIT){
    thisSol.push_back(sExp->cast<IntLit>()->v().toInt());
  }else if(sExp->eid()==Expression::E_FLOATLIT){
    thisSol.push_back(sExp->cast<FloatLit>()->v().toDouble()); 
  }else if(sExp->eid()==Expression::E_BOOLLIT){
    thisSol.push_back(sExp->cast<BoolLit>()->v()); 
  }else if(sExp->eid()==Expression::E_SETLIT){
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
    }else if(sExp->eid() == Expression::E_ARRAYLIT){
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
  }else if(sExp->eid() == Expression::E_CALL){
    Call *cl = sExp->cast<Call>();
    // name of the function
    string fnName = cl->id().c_str();
    if(fnName.compare("array1d") != 0 && fnName.compare("array2d") != 0  &&
       fnName.compare("array3d") != 0 && fnName.compare("array4d") != 0  &&
       fnName.compare("array5d") != 0 && fnName.compare("array6d") != 0){
        Rcpp::stop("function call not supported");
    }
    
    List ArrVec;
    NumericVector dimVec;
    int idflag = 0;
    for(int m = 0; m<cl->n_args();m++){
      if(cl->arg(m)->eid() == Expression::E_ARRAYLIT){
        //number of elements in the array
        int vec_size = cl->arg(m)->cast<ArrayLit>()->getVec().size();
        for(int p = 0; p < vec_size; p++ ){
          // get the expression of each element
          Expression *exp = cl->arg(m)->cast<ArrayLit>()->getVec().operator[](p);
          helper_sol_parse(exp, ArrVec);
        }
      }else if(cl->arg(m)->eid() == Expression::E_SETLIT){
        SetLit *sl = cl->arg(m)->cast<SetLit>();
        if(sl->isv()!= NULL){  
          int max_val =  sl->isv()->max().toInt();
          int min_val = sl->isv()->min().toInt();  
          // dimensions will be stored as 0 to max_val-min_val+1
          dimVec.push_back(max_val-min_val+1);
        }else if(sl->fsv() != NULL){
          Rcpp::stop("only integer set range allowed for arrays"); 
        }else{
          ASTExprVec<Expression> expVec = sl->v(); 
          dimVec.push_back(expVec.size());
        }
      }else if(cl->arg(m)->eid() == Expression::E_ID){
        idflag = 1;
        Rcpp::warning("Couldn't get the length of array dimensions -- returning as 1D array");
      }else{
        Rcpp::stop("arraynd function argument not supported");
      }
    }
    List retArrVec;
    if(idflag != 1 && dimVec.length() > 1){
      int getSize = 1;
      for (int k =0; k < dimVec.length(); k++){
        getSize = getSize*dimVec[k];
      }
      if(getSize != ArrVec.length()) Rcpp::stop("length of array values and dimensions doesn't match");
      for(int dV = 0; dV < dimVec.length(); dV++){
        List dAV;
        for(int k = 0; k < dimVec[dV]; k++){
          dAV.push_back(ArrVec[k]);
        } 
        retArrVec.push_back(dAV);
      }
    }else{
      retArrVec = ArrVec;
    }
    thisSol.push_back(retArrVec);      
  }else{
    Rcpp::stop("solution parsing not supported");
  }
      
}
