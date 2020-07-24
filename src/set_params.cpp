#include <Rcpp.h>
#include <fstream>
#include <minizinc/prettyprinter.hh>
#include "pathStringcheck.h"
#include "helper_parse.h"


using namespace std;
using namespace Rcpp;
using namespace MiniZinc;
  
//' @title assign the missing parameters to the model
//' 
//' @desciption the missing parameters found from parse_mzn can be assigned 
//' in this function
//' 
//' @importFrom Rcpp sourceCpp
//' @export set_params
//' @useDynLib rminizinc, .registration=TRUE
//' @param modData list containing the parameter values.
//' @param modelString string representation of the MiniZinc model
//' @param mznpath path of the mzn file to read the model
//' @param modify_mzn if the user wants to modify the mzn parameters.
// [[Rcpp::export]]
std::string set_params(List modData, std::string modelString = "",
                       std::string mznpath = "", bool modify_mzn = false) {
  
  modelString  = pathStringcheck(modelString, mznpath);
  Model *model = helper_parse(modelString,  "set_params.mzn");
  
  vector<Item*> items;
  NumericVector nameIndexMap; 
  CharacterVector parNames;
  int s = model->size();
  if(s == 0){
    Rcpp::stop("Empty model!");
  }
  for(int i = 0; i<s;i++){
    items.push_back(model->operator[] (i));
    if(items[i]->iid() == Item::II_VD){
      if(items[i]->cast<VarDeclI>()->e()->e() == NULL && 
         items[i]->cast<VarDeclI>()->e()->type().ispar()){
        nameIndexMap.push_back(i);
        parNames.push_back(items[i]->cast<VarDeclI>()->e()->id()->str().aststr()->c_str());
      }
    }else if (items[i]->iid() == Item::II_ASN){
      int index = nameIndexMap.offset(items[i]->cast<AssignI>()->id().str());
      nameIndexMap.erase(index);
    }
  }
  
  nameIndexMap.names() = parNames;
  if(modData.length() == 0 || modData.length() > nameIndexMap.length()){
    Rcpp::stop("Provide the values for atleast 1 to the total number missing parameters");
  }else{
    if(Rf_isNull(modData.names())){
      Rcpp::stop("Please provide names for the values");
    }
    CharacterVector argParNames = modData.names();
    for(int i = 0;i< modData.length();i++){
      if(!nameIndexMap.containsElementNamed(argParNames[i])){
        string incorrectNameError = string(argParNames[i]);
        incorrectNameError.append(" is either already initialized or not a parameter");
        Rcpp::stop(incorrectNameError);
      }
    }
    for(int i = 0; i < modData.length();i++){
      // get the corresponding item
      int index = nameIndexMap.offset(string(argParNames[i]));
      VarDecl *vd = items[nameIndexMap[index]]->cast<VarDeclI>()->e();
      Type tp = items[nameIndexMap[index]]->cast<VarDeclI>()->e()->type();
      if(tp.isint()){ 
        vd->e(IntLit::a(IntVal((int)modData[i])));
      }else if(tp.isfloat()){
        vd->e(FloatLit::a(FloatVal((float)modData[i])));
      }else if(tp.isbool()){
        BoolLit *bl = new BoolLit(items[nameIndexMap[index]]->loc(),(bool)modData[i]);
        vd->e(bl);
      }else if(tp.is_set()){
        vector<Expression*> expVec;
        NumericVector setVal = modData[i];
        if(tp.isintset()){
          for(int it = 0;it<setVal.length();it++)
            expVec.push_back(IntLit::a(setVal[it]));
        }else if(tp.isfloatset()){
          for(int it = 0;it<setVal.length();it++)
            expVec.push_back(FloatLit::a(setVal[it]));
        }else if(tp.isboolset()){
          for(int it = 0;it<setVal.length();it++)
            expVec.push_back(new BoolLit(items[nameIndexMap[index]]->loc(),(bool)modData[it]));
        }
        SetLit *sl = new SetLit(items[nameIndexMap[i]]->loc(), expVec);
        vd->e(sl);
      }else if(tp.dim() >= 1  && !tp.is_set()){
        // arrays of all dimensions
        vector<Expression*> callVec;
        string arrayfnName = "array";
        arrayfnName.append(to_string(tp.dim()));
        arrayfnName.append("d");
        
        TypeInst *nti = items[nameIndexMap[index]]->cast<VarDeclI>()->e()->ti();
        ASTExprVec<TypeInst> index_ti = nti->ranges();
        int noIndex = index_ti.size();
        for( int k = 0; k<noIndex; k++){
          Expression *ind_exp = index_ti.operator[](k);
          callVec.push_back(ind_exp);
        }
        vector<Expression*> expVec;
        if(tp.st() == Type::ST_PLAIN && tp.ot() == Type::OT_PRESENT && tp.bt() == Type::BT_INT){
            // 1 dimensional integer array
            NumericVector arrVal= modData[i];
            for(int it = 0;it < arrVal.length();it++)
              expVec.push_back(IntLit::a(arrVal[it]));
        }else if(tp.st() == Type::ST_PLAIN && tp.ot() == Type::OT_PRESENT && tp.bt() == Type::BT_UNKNOWN){
            // array index is not a value but a variable
            NumericVector arrVal= modData[i];
            for(int it = 0;it < arrVal.length();it++)
              expVec.push_back(IntLit::a(arrVal[it]));
          }else if(tp.st() == Type::ST_PLAIN && tp.ot() == Type::OT_PRESENT && tp.bt() == Type::BT_BOOL){
            // 1 dimensional bool array
            LogicalVector arrVal= modData[i];
            for(int it = 0;it < arrVal.length();it++)
              expVec.push_back(new BoolLit(items[nameIndexMap[index]]->loc(), arrVal[it]));
          }else if(tp.st() == Type::ST_PLAIN && tp.ot() == Type::OT_PRESENT && tp.bt() == Type::BT_STRING){
            // array of string
            StringVector arrStrVal = modData[i];
            for(int it = 0;it < arrStrVal.length();it++){
              expVec.push_back(new StringLit(items[nameIndexMap[index]]->loc(),(string)arrStrVal[it]));   
            }
          }
          ArrayLit *al = new ArrayLit(items[nameIndexMap[index]]->loc(),expVec);
          // initialize constructor for 1 dimensional arrays
          if(callVec.size()){
            callVec.push_back(al);
            Call *ncall = new Call(items[nameIndexMap[index]]->loc(),arrayfnName, callVec);
            vd->e(ncall);     
          }else{
            // index sets not available 
            Rcpp::stop("Array index not found");
          }
      }else{
             Rcpp::stop("Parameter can't be set --  not supported");
           }
        }
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

