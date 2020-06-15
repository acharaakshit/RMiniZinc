#include <Rcpp.h>
#include <minizinc/parser.hh>
#include <minizinc/prettyprinter.hh>

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
//' @param modelString string representation of the MiniZinc model.
//' @param modelStringName the name of model string.
//' @param mznfilename the name of model.
//' @param dznfilename the name of the dzn file.
// [[Rcpp::export]]
std::string mzn_parse(std::string modelString, std::string  modelStringName,
                        std::vector<std::string> mznfilename,
                        std::vector<std::string> dznfilename){
  // create a model and parse its items (make modifications to the model -- to be done)
  Model* model;
  if(modelString.empty() && mznfilename.empty()){
    return "PROVIDE EITHER modelString OR mznfilename";
  }else{ 
    Env* env = new Env();
    vector<string> ip = {};
    ostream& os = cerr;
    if(!mznfilename.empty()){
      //use parse
      model = MiniZinc::parse(*env, mznfilename, dznfilename, modelString, modelStringName,
                              ip, true, true, true, os);
    }else{
      // use parsefromString
      vector<SyntaxError> se;
      model = MiniZinc::parseFromString(*env, modelString, modelStringName , ip, true, true, true, os, se);
    }}
  int s = (model-> size());
  cout << "The number of items in the model are " << s << endl;
  vector<Item*> items;
  int type = 0;
  // to store the variable names
  string name;
  for(int i=0; i < s; i++){
    items.push_back(model->operator[] (i));
    switch(items[i]->iid()){
    case Item::II_VD:
      // decision variables or parameters
      name = items[i]->cast<VarDeclI>()->e()->id()->str().aststr()->c_str();
      if((items[i]->cast<VarDeclI>()->e()->e() == NULL &&  items[i]->cast<VarDeclI>()->e()->type().ispar()) || items[i]->cast<VarDeclI>()->e()->type().isvar()){
        Type tp = items[i]->cast<VarDeclI>()->e()->type();
        string tp_string = "";
        if(tp.isvar()){
          tp_string = "decision variable" ; 
        }else {
          tp_string = "parameter" ;
        }
        if(tp.isint()){
          cout << "item " << name << " is an integer " << tp_string << " declaration" << endl;
        }else if(tp.isfloat()){
          cout << "item " << name << " is a float " << tp_string <<  " declaration" << endl;
        }else if(tp.isbool()){
          cout << "item " << name << " is a bool " << tp_string << " declaration" << endl;
        }else if(tp.is_set()){
          if(tp.isintset()){
            cout << "item " << name << " is a" << tp_string << " declaration for an integer set" << endl;
          }else if(tp.isfloatset()){
            cout << "item " << name << " is a float" <<  tp_string << " declaration for a float set" << endl;
          }else if(tp.isboolset()){
            cout << "item " << name << " is a bool" << tp_string << " declaration for a boolean set" << endl;
          }}else{
          if(tp.isintarray()){
            cout << "item " << name << " is an " << tp_string << " declaration for an integer array" << endl;
          }else if(tp.isintsetarray()){
            cout << "item " << name << " is a float" <<  tp_string << " declaration for an integer set array" << endl;
          }else if(tp.isboolarray()){
            cout << "item " << name << " is a bool" << tp_string << " declaration for a boolean array" << endl;
          }else{
            
              if(items[i]->cast<VarDeclI>()->e()->ti()->domain() != NULL){
                // variable has a domain
                if(items[i]->cast<VarDeclI>()->e()->ti()->domain()->cast<SetLit>()->isv() != NULL){
                  // integer set value
                  cout << "The maximum value of domain of " <<  name << " is: ";
                  cout << items[i]->cast<VarDeclI>()->e()->ti()->domain()->cast<SetLit>()->isv()->max()<<  endl;
                  cout << "The minimum value of domain of " << name << " is: ";
                  cout << items[i]->cast<VarDeclI>()->e()->ti()->domain()->cast<SetLit>()->isv()->min()<<  endl;  
                }else{
                  // float set value
                  cout << "The maximum value of domain of item " <<  name << " is: ";
                  cout << items[i]->cast<VarDeclI>()->e()->ti()->domain()->cast<SetLit>()->fsv()->max()<<  endl;
                  cout << "The minimum value of domain of item " << name << " is: ";
                  cout << items[i]->cast<VarDeclI>()->e()->ti()->domain()->cast<SetLit>()->fsv()->min()<<  endl;
                }
            }
          }	
        }
        continue;
      }
      // the name of the parameters	
      cout << "The name of variable "<< name << " is " << items[i]->cast<VarDeclI>()->e()->id()->v() << endl;
      type =  items[i]->cast<VarDeclI>()->e()->e()->eid();
      switch(type){
      case Expression::E_INTLIT:
        cout << "item " << name << " is an integer parameter initialization" << endl;
        items[i]->cast<VarDeclI>()->e()->e()->unboxedIntToIntVal().toInt();
        break;
      case Expression::E_FLOATLIT:
        cout << "item " << name << " is a float parameter initialization" << endl;
        items[i]->cast<VarDeclI>()->e()->e()->unboxedFloatToFloatVal();
        break;
      case Expression::E_BOOLLIT:
        cout << "item " << name << " is a boolean parameter initialization" << endl;
        break;
      case Expression::E_SETLIT:
        cout << "item " << name << " is a set parameter initialization" << endl;
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
        cout << "item " << name << " is an array parameter initialization" << endl;
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
    case Item::II_OUT:
      //cout << "item number " << i << " is an output item" << endl;
      break;
    case Item::II_ASN:
      cout << "item " << items[i]->cast<AssignI>()->id().str()  << " is an assignment" << endl;
      break;
    default:;
      //cout << "Invalid input" << endl;
    }
  }
  // return the string representation of the model
  stringstream strmodel;
  Printer *p = new Printer(strmodel); 
  p->print(model);
  string mString = strmodel.str();
  return mString;
}


