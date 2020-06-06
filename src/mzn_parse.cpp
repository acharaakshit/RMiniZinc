#include <Rcpp.h>
#include "minizinc/parser.hh"


using namespace std;
using namespace MiniZinc;

using namespace Rcpp;

//' @title MiniZinc syntax parser
//' 
//' @description parses the MiniZinc syntax into R objects
//'
//' @importFrom Rcpp sourceCpp
//' @export parse_MiniZinc
//' @useDynLib rminizinc, .registration=TRUE
//' 

// [[Rcpp::export]]
NumericVector parse_MiniZinc(const char* modelString){
  
  Env* env = new Env();
  const char* modelStr = modelString;
  vector<string> ip = {};
  ostream& os = cerr;
  vector<SyntaxError> se;
  Model* model = MiniZinc::parseFromString(*env, modelStr, "mymodel.mzn" , ip, true, true, true, os, se);
  //SolverConfigs *sc = new SolverConfigs("/snap/minizinc/current/share/minizinc/solvers/gecode.msc");
  int s = (model-> size());
  cout << "The number of items in the model are " << s << endl;
  vector<Item*> items;
  MiniZinc::ItemVisitor* iv = new ItemVisitor();
  int type = 0;
  NumericVector retval = 0;
  for(int i=0; i < s; i++){
    items.push_back(model->operator[] (i));
    Location lc = items[i]->loc();
    switch(items[i]->iid()){
    case Item::II_VD:
      // the name of the variables	
      cout << "The name of variable "<< i + 1 << " is " << items[i]->cast<VarDeclI>()->e()->id()->v() << endl;
      type =  items[i]->cast<VarDeclI>()->e()->e()->eid();
      switch(type){
      case Expression::E_INTLIT:
        cout << "item " << i + 1 << " is an integer parameter" << endl;
        retval = items[i]->cast<VarDeclI>()->e()->e()->unboxedIntToIntVal().toInt();
        break;
      case Expression::E_FLOATLIT:
        cout << "item " << i + 1 << " is a float parameter" << endl;
        items[i]->cast<VarDeclI>()->e()->e()->unboxedFloatToFloatVal();
        break;
      case Expression::E_BOOLLIT:
        cout << "item " << i + 1 << " is a boolean parameter" << endl;
        break;
      case Expression::E_SETLIT:
        cout << "item " << i + 1 << " is a set parameter" << endl;
        cout << *(items[i]->cast<VarDeclI>()->e()->e()->cast<SetLit>()->isv());
        break;
      case Expression::E_ARRAYLIT:
        cout << "item " << i + 1 << " is an array parameter" << endl;
        break;
      case Expression::E_ARRAYACCESS:
        break;
      default:;
        //cout << "invalid variable type " << endl;
      }
      
      break;
    case Item::II_CON: 
      items[i]->cast<ConstraintI>()->e();
      items[i]->cast<ConstraintI>()->e()->Expression::eid();
      break;
    case Item::II_SOL:
      items[i]->cast<SolveI>();
      break;
    default:;
      //cout << "Invalid input" << endl;
    }
  }
  //print(retval);
  return retval;
}


