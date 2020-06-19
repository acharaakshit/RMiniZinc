#include <Rcpp.h>
#include <fstream>
#include <minizinc/parser.hh>
#include <minizinc/prettyprinter.hh>
#include "filetoString.h"


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
//' @param mzpath path of the mzn file 
// [[Rcpp::export]]
std::string set_params(List modData, std::string modelString = "",
                       std::string mznpath = "") {
  Env* env = new Env();
  vector<string> ip = {};
  ostringstream os;
  vector<SyntaxError> se;
  Model *model;
  if(modelString.empty() && mznpath.empty()){
    Rcpp::stop("PROVIDE EITHER modelString OR mznfilename");
  }else if(!modelString.empty() && !mznpath.empty()){
    Rcpp::stop("PROVIDE ONLY ONE OF modelString OR mznfilename");
  }else if(mznpath.length()){
    //convert to string 
    modelString = filetoString(mznpath);
  }
  try{
    string modelStringName = "model.mzn";
    model = MiniZinc::parseFromString(*env, modelString, modelStringName , ip, true, true, true, os, se);
    if(model==NULL) throw std::exception();
    else if(se.size()){
      string syntaxErrors;
      for(int i = 0;i < se.size();i++){
        syntaxErrors.append(se[i].what());
        syntaxErrors.append("\n");
      }
      Rcpp::stop(syntaxErrors);
    }
  }catch(std::exception& e){
    string parseError;
    parseError = os.str();
    Rcpp::stop(parseError);
  }
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
              ArrayLit *al = new ArrayLit(items[nameIndexMap[index]]->loc(),expVec);
              vd->e(al);
            }
          }else if(tp.isintsetarray()){
            //
          }else if(tp.isboolarray()){
            //
          }
        }	
    }
  }
  stringstream strmodel;
  Printer *p = new Printer(strmodel); 
  p->print(model);
  string mString = strmodel.str();
  
  if(!mznpath.empty()){
    ofstream out(mznpath);
    out << mString;
    out.close();
  }
  
  return mString;
}

