#include <Rcpp.h>
#include <fstream>
#include <minizinc/prettyprinter.hh>
#include "filetoString.h"
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
  
  if(modelString.empty() && mznpath.empty()){
    Rcpp::stop("PROVIDE EITHER modelString OR mznfilename");
  }else if(!modelString.empty() && !mznpath.empty()){
    Rcpp::stop("PROVIDE ONLY ONE OF modelString OR mznfilename");
  }else if(mznpath.length()){
    // check file extension
    if(!(mznpath.substr(mznpath.find_last_of(".") + 1) == "mzn" ))
      Rcpp::stop("file extention is not mzn");
    //convert to string 
    modelString = filetoString(mznpath);
  }
  
  Model *model = helper_parse(modelString, "set_params.mzn");
  
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
  if(modData.length() != 0 && modData.length() > nameIndexMap.length()){
    Rcpp::stop("Provide the values for atleast 1 to the total number missing parameters");
  }else if(modData.length()){
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
      }else{
        vector<Expression*> expVec;
        if(tp.dim() == 1){
        if(tp.isintarray()){
            // 1 dimensional integer array
            NumericVector arrVal= modData[i];
            for(int it = 0;it < arrVal.length();it++)
              expVec.push_back(IntLit::a(arrVal[it]));
          }else if(tp.isintsetarray()){
            // 1 integer array of set
          }else if(tp.isboolarray()){
            // 1 dimensional bool array
          }else if(tp.dim() == 1 && tp.st() == Type::ST_PLAIN && tp.ot() == Type::OT_PRESENT && tp.bt() == Type::BT_STRING){
            // array of string
            StringVector arrStrVal = modData[i];
            for(int it = 0;it < arrStrVal.length();it++){
              expVec.push_back(new StringLit(items[nameIndexMap[index]]->loc(),(string)arrStrVal[it]));   
            }
          }
          // initialize constructor for 1 dimensional arrays
          ArrayLit *al = new ArrayLit(items[nameIndexMap[index]]->loc(),expVec);
          vd->e(al);  
        }else if(tp.dim() == 2 && tp.st() == Type::ST_PLAIN && tp.ot()==Type::OT_PRESENT){
            // set two dimensional array values
             vector<vector<Expression*>> exVec;
             NumericMatrix arr2dVal = modData[i];
             for(int it = 0;it<arr2dVal.rows();it++){
               vector<Expression*> expVec;
               NumericVector _1dVal = arr2dVal(it, _);
               for(int itt = 0;itt < _1dVal.length();itt++)
                 if(tp.bt()==Type::BT_INT )
                    expVec.push_back(IntLit::a(_1dVal[itt]));
                 else if(tp.bt()==Type::BT_FLOAT)
                   expVec.push_back(FloatLit::a(_1dVal[itt]));
                 else if(tp.bt() == Type::BT_UNKNOWN)
                   // needs to be handled properly
                   expVec.push_back(IntLit::a(_1dVal[itt]));
               exVec.push_back(expVec);
             }
             ArrayLit *al = new ArrayLit(items[nameIndexMap[index]]->loc(),exVec);
             vd->e(al); 
          }else{
             Rcpp::stop("Parameter can't be set --  not supported");
           }
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
  }
  
  return mString;
}

