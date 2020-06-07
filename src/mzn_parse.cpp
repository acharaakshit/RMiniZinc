#include <Rcpp.h>
#include <minizinc/parser.hh>


using namespace std;
using namespace MiniZinc;

using namespace Rcpp;

//' @title MiniZinc syntax parser
//' 
//' @description parses the MiniZinc syntax into R objects
//'
//' @importFrom Rcpp sourceCpp
//' @export mzn_parse
//' @useDynLib rminizinc, .registration=TRUE
//' @param modelString string representation of the MiniZinc model
// [[Rcpp::export]]
NumericVector mzn_parse(const char* modelString){
  
  Env* env = new Env();
  const char* modelStr = modelString;
  vector<string> ip = {};
  ostream& os = cerr;
  vector<SyntaxError> se;
  Model* model = MiniZinc::parseFromString(*env, modelStr, "mymodel.mzn" , ip, true, true, true, os, se);
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
      // decision variables or parameters
      items[i]->cast<VarDeclI>()->e()->type();
      if((items[i]->cast<VarDeclI>()->e()->e() == NULL &&  items[i]->cast<VarDeclI>()->e()->type().ispar()) || items[i]->cast<VarDeclI>()->e()->type().isvar()){
        Type tp = items[i]->cast<VarDeclI>()->e()->type();
        string tp_string = "";
        if(tp.isvar()){
          tp_string = "decision variable" ; 
        }else {
          tp_string = "parameter" ;
        }
        if(tp.isint()){
          cout << "item " << i << " is an integer " << tp_string << " declaration" << endl;
        }else if(tp.isfloat()){
          cout << "item " << i << " is a float " << tp_string <<  " declaration" << endl;
        }else if(tp.isbool()){
          cout << "item " << i << " is a bool " << tp_string << " declaration" << endl;
        }else if(tp.is_set()){
          if(tp.isintset()){
            cout << "item " << i << " is a" << tp_string << " declaration for an integer set" << endl;
          }else if(tp.isfloatset()){
            cout << "item " << i << " is a float" <<  tp_string << " declaration for a float set" << endl;
          }else if(tp.isboolset()){
            cout << "item " << i << " is a bool" << tp_string << " declaration for a boolean set" << endl;
          }else{
            cout << "unknown set type";
          }
        }
        else{
          if(tp.isintarray()){
            cout << "item " << i << " is an " << tp_string << " declaration for an integer array" << endl;
          }else if(tp.isintsetarray()){
            cout << "item " << i << " is a float" <<  tp_string << " declaration for an integer set array" << endl;
          }else if(tp.isboolarray()){
            cout << "item " << i << " is a bool" << tp_string << " declaration for a boolean array" << endl;
          }else{
            cout << "unknown array type";
          }	
        }
        if(items[i]->cast<VarDeclI>()->e()->ti()->domain() != NULL){
          // variable has a domain
          if(items[i]->cast<VarDeclI>()->e()->ti()->domain()->cast<SetLit>()->isv() != NULL){
            // integer set value
            cout << "The maximum value of domain of item" << i << " is: ";
            cout << items[i]->cast<VarDeclI>()->e()->ti()->domain()->cast<SetLit>()->isv()->max()<<  endl;
            cout << "The minimum value of domain of item" << i << " is: ";
            cout << items[i]->cast<VarDeclI>()->e()->ti()->domain()->cast<SetLit>()->isv()->min()<<  endl;  
          }else{
            // float set value
            cout << "The maximum value of domain of item" << i << " is: ";
            cout << items[i]->cast<VarDeclI>()->e()->ti()->domain()->cast<SetLit>()->fsv()->max()<<  endl;
            cout << "The minimum value of domain of item" << i << " is: ";
            cout << items[i]->cast<VarDeclI>()->e()->ti()->domain()->cast<SetLit>()->fsv()->min()<<  endl;
          }
          
        }		
        
        continue;
      }
      // the name of the parameters	
      cout << "The name of variable "<< i + 1 << " is " << items[i]->cast<VarDeclI>()->e()->id()->v() << endl;
      type =  items[i]->cast<VarDeclI>()->e()->e()->eid();
      switch(type){
      case Expression::E_INTLIT:
        cout << "item " << i + 1 << " is an integer parameter initialization" << endl;
        retval = items[i]->cast<VarDeclI>()->e()->e()->unboxedIntToIntVal().toInt();
        break;
      case Expression::E_FLOATLIT:
        cout << "item " << i + 1 << " is a float parameter initialization" << endl;
        items[i]->cast<VarDeclI>()->e()->e()->unboxedFloatToFloatVal();
        break;
      case Expression::E_BOOLLIT:
        cout << "item " << i + 1 << " is a boolean parameter initialization" << endl;
        break;
      case Expression::E_SETLIT:
        cout << "item " << i + 1 << " is a set parameter initialization" << endl;
        if(items[i]->cast<VarDeclI>()->e()->e()->cast<SetLit>()->isv()!=NULL){
          // integer set
          cout << "max possible value " <<  items[i]->cast<VarDeclI>()->e()->e()->cast<SetLit>()->isv()->max();
          cout << "min possible value " << items[i]->cast<VarDeclI>()->e()->e()->cast<SetLit>()->isv()->min();  
        }else{
          // floating point set
          cout << "max possible value " <<  items[i]->cast<VarDeclI>()->e()->e()->cast<SetLit>()->fsv()->max();
          cout << "min possible value " << items[i]->cast<VarDeclI>()->e()->e()->cast<SetLit>()->fsv()->min();
        }
        
        break;
      case Expression::E_ARRAYLIT:
        cout << "item " << i + 1 << " is an array parameter initialization" << endl;
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


