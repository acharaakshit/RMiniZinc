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
//' @param mznfilename the name of model.
//' @param dznfilename the name of the dzn file.
//' @param modelStringName the name of model string.
//' @param modData list containing the parameter values.
// [[Rcpp::export]]
std::string mzn_parse(List modData, std::string modelString = "", 
                      std::string mznfilename = "",
                      std::string dznfilename = "", 
                      std::string  modelStringName = "abc.mzn"){
  // create a model and parse its items (make modifications to the model -- to be done)
  Model* model;
  if(modelString.empty() && mznfilename.empty()){
    Rcpp::stop("PROVIDE EITHER modelString OR mznfilename");
  }else{ 
    Env* env = new Env();
    vector<string> ip = {};
    ostringstream os;
    if(mznfilename.length()){
      //use parse
      std::vector<std::string> datafiles; 
      if(dznfilename.length() > 0){
          datafiles.push_back(dznfilename);  
      }
      std::vector<std::string> filename;
      filename.push_back(mznfilename);
      try{
        model = MiniZinc::parse(*env, filename, datafiles, modelString, modelStringName,
                              ip, true, true, true, os);
        if(model==NULL) throw std::exception();
      }catch(std::exception& e){
        string parseError;
        parseError = os.str();
        Rcpp::stop(parseError);
      }
    }else{
      // use parsefromString
      vector<SyntaxError> se;
      try{
        model = MiniZinc::parseFromString(*env, modelString, modelStringName , ip, true, true, true, os, se);
        if(model==NULL) throw std::exception();
        else if(se.size()){
          Rcpp::stop(se[0].what());
        }
      }catch(std::exception& e){
        string parseError;
        parseError = os.str();
        Rcpp::stop(parseError);
      }
    }}

  // size of the model
  int s = model-> size();
  // get all the items and the names of all the parameters and map to the item numbers
  vector<Item*> items;
  NumericVector nameValMap; 
  CharacterVector parNames;
  for(int i = 0; i<s;i++){
    items.push_back(model->operator[] (i));
    if(items[i]->iid() == Item::II_VD){
      if(items[i]->cast<VarDeclI>()->e()->e() == NULL && 
         items[i]->cast<VarDeclI>()->e()->type().ispar()){
      nameValMap.push_back(i);
      parNames.push_back(items[i]->cast<VarDeclI>()->e()->id()->str().aststr()->c_str());
      }
    }
  }
  if(modData.length() != 0 && modData.length() != nameValMap.length()){
    return "Provide the values for all the declared parameters";
  }else if(modData.length()){
    nameValMap.names() = parNames;
    CharacterVector modDataNames = modData.names();
    for(int i = 0; i < modData.length();i++){
      if(modDataNames[i] == parNames[i] ){
        VarDecl *vd = items[nameValMap[i]]->cast<VarDeclI>()->e();
        Type tp = items[nameValMap[i]]->cast<VarDeclI>()->e()->type();
        if(tp.isint()){ 
          vd->e(IntLit::a(IntVal((int)modData[i])));
        }else if(tp.isfloat()){
          vd->e(FloatLit::a(FloatVal((float)modData[i])));
        }else if(tp.isbool()){
          BoolLit *bl = new BoolLit(items[nameValMap[i]]->loc(),(bool)modData[i]);
          vd->e(bl);
        }else if(tp.is_set()){
          if(tp.isintset()){
            //vector<Expression*> expVec;
            //NumericVector setVal = modData[i];
            //for(int it = 0;it<setVal.length();it++)
            //  expVec.push_back(IntSetVal::a());
            //SetLit *sl = new SetLit(items[nameValMap[i]]->loc(), expVec)
          }else if(tp.isfloatset()){
            
          }else if(tp.isboolset()){
            
          }}else{
            if(tp.isintarray()){
                if(tp.dim() == 1){
                  // 1 dimensional array
                vector<Expression*> expVec;
                  NumericVector arrVal= modData[i];
                  for(int it = 0;it < arrVal.length();it++)
                    expVec.push_back(IntLit::a(arrVal[it]));
                  ArrayLit *al = new ArrayLit(items[nameValMap[i]]->loc(),expVec);
                  vd->e(al);
                }
            }else if(tp.isintsetarray()){
              //
            }else if(tp.isboolarray()){
              //
            }else{
              
              if(items[i]->cast<VarDeclI>()->e()->ti()->domain() != NULL){
                // variable has a domain
              }else{
                // float domain
              }
            }
          }	
      }
    }
  }
  
  int type = 0;
  // to store the variable names
  string name;
  for(int i=0; i < s; i++){
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
        continue;
      }
      // the name of the parameters	
      cout << "The name of variable "<< name << " is " << items[i]->cast<VarDeclI>()->e()->id()->v() << endl;
      type =  items[i]->cast<VarDeclI>()->e()->e()->eid();
      switch(type){
      case Expression::E_INTLIT:
        cout << "item " << name << " is an integer parameter with value: ";
        cout << items[i]->cast<VarDeclI>()->e()->e()->unboxedIntToIntVal().toInt() << endl;
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


