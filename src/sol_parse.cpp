#include <Rcpp.h>
#include <minizinc/parser.hh>


using namespace Rcpp;
using namespace MiniZinc;
using namespace std;

#define SOL_ERROR 404

//' @title parse the solution
//' 
//' @description can parse the solution of a model
//' 
//' @importFrom Rcpp sourceCpp
//' @export sol_parse
//' @useDynLib rminizinc, .registration=TRUE
//' @param solutionString solution of the model as a string representation
// [[Rcpp::export]]
NumericVector sol_parse(std::string solutionString) {
  Env* env = new Env();
  vector<string> ip = {};
  ostream& os = cerr;
  vector<SyntaxError> se;
  Model *model = MiniZinc::parseFromString(*env, solutionString, "",  ip, true, true, true, os, se);
  int s = (model-> size());
  string fname;
  NumericVector retval;
  vector<Item*> items;
  for(int i=0; i < s; i++){
    items.push_back(model->operator[] (i));
    bool isAssignment = (int)items[i]->iid()==Item::II_ASN?true:false;
    if(isAssignment){
      int type = items[i]->cast<AssignI>()->e()->eid();
      switch(type){
      case Expression::E_INTLIT:
        retval.push_back(items[i]->cast<AssignI>()->e()->cast<IntLit>()->v().toInt());
        break;
      case Expression::E_FLOATLIT:
        retval.push_back(items[i]->cast<AssignI>()->e()->cast<FloatLit>()->v().toDouble());
        break;
      case Expression::E_BOOLLIT:
        retval.push_back(items[i]->cast<AssignI>()->e()->cast<BoolLit>()->v());
        break;
      case Expression::E_SETLIT:
        if(items[i]->cast<AssignI>()->e()->cast<SetLit>()->isv()!=NULL){
          // integer set
          int max_val =  items[i]->cast<AssignI>()->e()->cast<SetLit>()->isv()->max().toInt();
          int min_val = items[i]->cast<AssignI>()->e()->cast<SetLit>()->isv()->min().toInt();  
          //retval.push_back(c(max_val,min_val));
          NumericVector snvec = {(double)max_val, (double)min_val}; 
        }else{
          // floating point set
          float max_val =  items[i]->cast<AssignI>()->e()->cast<SetLit>()->fsv()->max().toDouble();
          float min_val = items[i]->cast<AssignI>()->e()->cast<SetLit>()->fsv()->min().toDouble();
          //retval.push_back(c(max_val,min_val));
        }
        
        break;
      case Expression::E_ARRAYLIT:
        items[i]->cast<AssignI>()->e()->cast<ArrayLit>()->getVec().vec();
        break;
      case Expression::E_CALL:
        // name of the decision variable
        items[i]->cast<AssignI>()->e()->cast<Call>()->id().c_str();
        for(int m = 1;m<items[i]->cast<AssignI>()->e()->cast<Call>()->n_args();m++){
          int fntype = items[i]->cast<AssignI>()->e()->cast<Call>()->arg(m)->eid();
          // apply switch case again 
          if(fntype==Expression::E_ARRAYLIT){
              //number of elements in the array
              int vec_size = items[i]->cast<AssignI>()->e()->cast<Call>()->arg(m)->cast<ArrayLit>()->getVec().size();
              for(int p = 0;p < vec_size; p++ ){
                // get the expression form of each element
                Expression *exp = items[i]->cast<AssignI>()->e()->cast<Call>()->arg(m)->cast<ArrayLit>()->getVec().operator[](p);
                if(exp->isUnboxedInt()){
                  retval.push_back(exp->unboxedIntToIntVal().toInt());
                }else if(exp->isUnboxedFloatVal()){
                  retval.push_back(exp->unboxedFloatToFloatVal().toDouble());
                }
              }
          }else{
            // to be done
          }
        }
        break;
        default:;
      //cout << "invalid variable type " << endl;
      }
    }else {
      retval = SOL_ERROR;
      break;
      // "not a solution string";
    }
  }
  return retval;
 
}
