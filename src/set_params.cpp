#include <fstream>
#include <minizinc/prettyprinter.hh>
#include "helper_parse.h"

using namespace std;
using namespace Rcpp;
using namespace MiniZinc;
  
//' @title assign values to the missing parameters
//' 
//' @desciption missing parameters can be assigned 
//' values.
//' 
//' @importFrom Rcpp sourceCpp
//' @export set_params
//' @useDynLib rminizinc, .registration=TRUE
//' @param modData list containing the parameter values.
//' @param modelString string representation of the MiniZinc model
//' @param mznpath path of the mzn file to read the model
//' @param modify_mzn if the user wants to modify the mzn parameters.
//' @param includePath path of the included mzn in the model if it exists.
// [[Rcpp::export]]
std::string set_params(List modData, std::string modelString = "",
                       std::string mznpath = "", bool modify_mzn = false,
                       Nullable<std::vector<std::string>> includePath = R_NilValue) {
  
  
  modelString  = pathStringcheck(modelString, mznpath);
  vector<string> ip;
  if(!Rf_isNull(includePath)){
    ip = Rcpp::as<vector<string>>(includePath);
  }
  Model *model = helper_parse(modelString, "set_params.mzn", ip);  
  
  vector<Item*> items;
  NumericVector nameIndexMap; 
  CharacterVector parNames;

  for(int i = 0; i<model->size();i++){
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
        vd->e(new BoolLit(items[nameIndexMap[index]]->loc(),(bool)modData[i]));
      }else if(tp.isstring()){
        string sV = modData[i];
        StringLit *sl = new StringLit(items[nameIndexMap[index]]->loc(), sV);
      }else if(tp.bt() == Type::BT_UNKNOWN && !tp.is_set() && tp.dim() == 0){
        Type ntp = vd->ti()->domain()->type();
        if(ntp.isfloat()){
          vd->e(FloatLit::a(FloatVal((float)modData[i])));
        }else if(ntp.isbool()){
          vd->e(new BoolLit(items[nameIndexMap[index]]->loc(),(bool)modData[i]));
        }else if(ntp.isstring()){
          string sV = modData[i];
          StringLit *sl = new StringLit(items[nameIndexMap[index]]->loc(), sV);
        }else{
          if(ntp.isint()) 
            Rcpp::warning("Couldn't identify base data Type -- coercing to int");
          vd->e(IntLit::a(IntVal((int)modData[i])));
        }
      }else if(tp.is_set()){
        NumericVector setVal = modData[i];
        CharacterVector stNms = setVal.names();
        if(stNms.length()){
          vector<string> cmpWith = {"min", "max"};
          if(!std::equal(stNms.begin(), stNms.end(), cmpWith.begin())){
            Rcpp::stop("Named set value must have the names min and max");
          }
          SetLit *sl;
          if(tp.bt() == Type::BT_INT){
            IntSetVal *isv =  IntSetVal::a(IntVal((int)setVal[0]), IntVal((int)setVal[1]));
            sl = new SetLit(items[nameIndexMap[index]]->loc(), isv);
          }else if(tp.bt() == Type::BT_FLOAT){
            FloatSetVal *fsv =  FloatSetVal::a(FloatVal((double)setVal[0]), FloatVal((double)setVal[1]));
            sl = new SetLit(items[nameIndexMap[index]]->loc(), fsv);
          }else{
            Rcpp::stop("Only int and float Set ranges can be assigned");
          }
          vd->e(sl);
        }else{
          vector<Expression*> expVec;
          if(tp.isintset()){
            for(int it = 0;it<setVal.length();it++)
              expVec.push_back(IntLit::a(setVal[it]));
          }else if(tp.isfloatset()){
            for(int it = 0;it<setVal.length();it++)
              expVec.push_back(FloatLit::a(setVal[it]));
          }else if(tp.isboolset()){
            for(int it = 0;it<setVal.length();it++)
              expVec.push_back(new BoolLit(items[nameIndexMap[index]]->loc(),(bool)modData[it]));
          }else if(tp.bt() == Type::BT_STRING && tp.is_set()){
            for(int it = 0;it<setVal.length();it++){
              string setStr = modData[it];
              expVec.push_back(new StringLit(items[nameIndexMap[index]]->loc(), setStr)); 
            }
          }else{
            Rcpp::stop("not supported yet");
          }
          SetLit *sl = new SetLit(items[nameIndexMap[i]]->loc(), expVec);
          vd->e(sl);
        }
      }else if(tp.dim() >= 1  && !tp.is_set()){
        // arrays of all dimensions
        vector<Expression*> callVec;
        string arrayfnName = "array";
        arrayfnName.append(to_string(tp.dim()));
        arrayfnName.append("d");
        
        TypeInst *nti = items[nameIndexMap[index]]->cast<VarDeclI>()->e()->ti();
        ASTExprVec<TypeInst> index_ti = nti->ranges();
        for( int k = 0; k < index_ti.size(); k++){
          Expression *ind_exp = index_ti.operator[](k);
          callVec.push_back(ind_exp);
        } 
        vector<Expression*> expVec;
        if(tp.st() == Type::ST_PLAIN && tp.ot() == Type::OT_PRESENT && tp.bt() == Type::BT_INT){
            // 1 dimensional integer array
            IntegerVector arrVal= modData[i];
            for(int it = 0;it < arrVal.length();it++)
              expVec.push_back(IntLit::a(arrVal[it]));
        }else if(tp.st() == Type::ST_PLAIN && tp.ot() == Type::OT_PRESENT && tp.bt() == Type::BT_UNKNOWN){
            Type ntp = vd->ti()->domain()->type();
            if(ntp.isfloat()){
              NumericVector arrVal= modData[i];
              for(int it = 0;it < arrVal.length();it++)
                expVec.push_back(IntLit::a(arrVal[it]));
            }else if(ntp.isbool()){
              LogicalVector arrVal= modData[i];
              for(int it = 0;it < arrVal.length();it++)
                expVec.push_back(new BoolLit(items[nameIndexMap[index]]->loc(), arrVal[it]));  
            }else if(ntp.isstring()){
              StringVector arrStrVal = modData[i];
              for(int it = 0;it < arrStrVal.length();it++){
                expVec.push_back(new StringLit(items[nameIndexMap[index]]->loc(),(string)arrStrVal[it]));   
              }
            }else{
              if(ntp.isint()) 
                Rcpp::warning("Couldn't identify base data Type -- coercing to int");
              IntegerVector arrVal= modData[i];
              for(int it = 0;it < arrVal.length();it++)
                expVec.push_back(IntLit::a(arrVal[it])); 
            }
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
          }else if(tp.st() == Type::ST_SET && tp.ot() == Type::OT_PRESENT){
            List ArrVal = modData[i];
            for(int it = 0; it< ArrVal.length();it++ ){
              NumericVector setVal = ArrVal[it];
              CharacterVector stNms = setVal.names();
              if(stNms.length()){
                vector<string> cmpWith = {"min", "max"};
                if(!std::equal(stNms.begin(), stNms.end(), cmpWith.begin())){
                  Rcpp::stop("Named set value must have the names min and max");
                }
                SetLit *sl;
                if(tp.bt() == Type::BT_INT){
                  IntSetVal *isv =  IntSetVal::a(IntVal((int)setVal[0]), IntVal((int)setVal[1]));
                  sl = new SetLit(items[nameIndexMap[index]]->loc(), isv);
                }else if(tp.bt() == Type::BT_FLOAT){
                  FloatSetVal *fsv =  FloatSetVal::a(FloatVal((double)setVal[0]), FloatVal((double)setVal[1]));
                  sl = new SetLit(items[nameIndexMap[index]]->loc(), fsv);
                }else{
                  Rcpp::stop("Only int and float Set ranges can be assigned");
                }
                expVec.push_back(sl);
              }else{
                vector<Expression*> subExpVec;
                if(tp.isintset()){
                  for(int itt = 0;itt<setVal.length();itt++)
                    subExpVec.push_back(IntLit::a(setVal[itt]));
                }else if(tp.isfloatset()){
                  for(int itt = 0;itt<setVal.length();itt++)
                    subExpVec.push_back(FloatLit::a(setVal[itt]));
                }else if(tp.isboolset()){
                  for(int itt = 0;itt<setVal.length();itt++)
                    subExpVec.push_back(new BoolLit(items[nameIndexMap[index]]->loc(),(bool)modData[itt]));
                }else if(tp.bt() == Type::BT_STRING && tp.is_set()){
                  for(int it = 0;it<setVal.length();it++){
                    string setStr = modData[it];
                    subExpVec.push_back(new StringLit(items[nameIndexMap[index]]->loc(), setStr)); 
                  }
                }else{
                  Rcpp::stop("not supported yet");
                }
                SetLit *sl = new SetLit(items[nameIndexMap[i]]->loc(), subExpVec);
                expVec.push_back(sl);
              }
            }
          }else{
            Rcpp::stop("Array type not supported yet");
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

