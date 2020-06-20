#include <Rcpp.h>
#include <regex>
#include <minizinc/parser.hh>



using namespace Rcpp;
using namespace MiniZinc;
using namespace std;

//' @title parse the solution
//' 
//' @description can parse the solution of a model
//' 
//' @importFrom Rcpp sourceCpp
//' @export sol_parse
//' @useDynLib rminizinc, .registration=TRUE
//' @param solutionString solution of the model as a string representation
// [[Rcpp::export]]
List sol_parse(std::string solutionString) {
  
  string delimiter = "----------";
  size_t pos = 0;
  static const size_t npos = -1;
  std::string token;
  vector<string> solutions;
  
  while ((pos = solutionString.find(delimiter)) != npos) {
    token = solutionString.substr(0, pos);
    solutions.push_back(token);
    solutionString.erase(0, pos + delimiter.length());
  }
  
  if(solutionString.find("=====UNSATISFIABLE=====") != npos){
    Rcpp::stop("No Solution");
  }else if(solutionString.find("=====ERROR=====") != npos){
    Rcpp::stop("Errored");
  }
  
  if(solutions.size()==0) Rcpp::stop("No solution seperator found-- incorrect solution string");
    
  int optimal_sol_flag = solutionString.find("==========") != npos ? 1:0;
  
  List retVal;
  CharacterVector nameretVal;
  
  for(int nsol=0; nsol< solutions.size(); nsol++){
    
    List thisSol;
    solutionString = solutions[nsol];
    CharacterVector varName;
    // create the model
    Env* env = new Env();
    vector<string> ip = {};
    ostringstream os;
    vector<SyntaxError> se;
    Model *model; 
    try{
      model = MiniZinc::parseFromString(*env, solutionString, "sol.mzn" , ip, true, true, true, os, se);
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
    int s = (model-> size());
    if(s==0) Rcpp::stop("empty model generated by solution string");
    
    vector<Item*> items;
    for(int i=0; i < s; i++){
      items.push_back(model->operator[] (i));
      bool isAssignment = (int)items[i]->iid()==Item::II_ASN?true:false;
      if(isAssignment){
        varName.push_back(items[i]->cast<AssignI>()->id().str());
        int type = items[i]->cast<AssignI>()->e()->eid();
        switch(type){
        case Expression::E_INTLIT:
          thisSol.push_back(items[i]->cast<AssignI>()->e()->cast<IntLit>()->v().toInt());
          break;
        case Expression::E_FLOATLIT:
          thisSol.push_back(items[i]->cast<AssignI>()->e()->cast<FloatLit>()->v().toDouble());
          break;
        case Expression::E_BOOLLIT:
          thisSol.push_back(items[i]->cast<AssignI>()->e()->cast<BoolLit>()->v());
          break;
        case Expression::E_SETLIT:
            if(items[i]->cast<AssignI>()->e()->cast<SetLit>()->isv()!= NULL){  
              int max_val =  items[i]->cast<AssignI>()->e()->cast<SetLit>()->isv()->max().toInt();
              int min_val = items[i]->cast<AssignI>()->e()->cast<SetLit>()->isv()->min().toInt();  
              IntegerVector setVec = {max_val, min_val}; 
              thisSol.push_back(setVec);
            }else if(items[i]->cast<AssignI>()->e()->cast<SetLit>()->fsv()!=NULL){
              float max_val =  items[i]->cast<AssignI>()->e()->cast<SetLit>()->fsv()->max().toDouble();
              float min_val = items[i]->cast<AssignI>()->e()->cast<SetLit>()->fsv()->min().toDouble();
              NumericVector setVec = {max_val, min_val};
              thisSol.push_back(setVec);
            }else{
              ASTExprVec<Expression> expVec = items[i]->cast<AssignI>()->e()->cast<SetLit>()->v();
              int expVec_size = expVec.size();
              List setVec;
              for(int p = 0; p < expVec_size; p++){
                Expression *exp = expVec.operator[](p);
                if(exp->isUnboxedInt()){
                  setVec.push_back((double)exp->unboxedIntToIntVal().toInt());
                }else if(exp->isUnboxedFloatVal()){
                  setVec.push_back(exp->unboxedFloatToFloatVal().toDouble());
                }else if(exp->eid() == Expression::E_BOOLLIT){
                  setVec.push_back(exp->cast<BoolLit>()->v());
                }
              }
              thisSol.push_back(setVec);
            }
          
          
          break;
        case Expression::E_ARRAYLIT:
          if(items[i]->cast<AssignI>()->e()->cast<ArrayLit>()->getVec().size()){
            int vec_size = items[i]->cast<AssignI>()->e()->cast<ArrayLit>()->getVec().size();
            List ArrVec;
            for(int p = 0;p < vec_size; p++ ){
              // get the expression form of each element
              Expression *exp = items[i]->cast<AssignI>()->e()->cast<ArrayLit>()->getVec().operator[](p);
              if(exp->isUnboxedInt()){
                ArrVec.push_back((double)exp->unboxedIntToIntVal().toInt());
              }else if(exp->isUnboxedFloatVal()){
                ArrVec.push_back(exp->unboxedFloatToFloatVal().toDouble());
              }else if(exp->eid() == Expression::E_BOOLLIT){
                ArrVec.push_back(exp->cast<BoolLit>()->v());
              }
            }
            thisSol.push_back(ArrVec);
          }  
          break;
        case Expression::E_CALL:
          // name of the function
          // items[i]->cast<AssignI>()->e()->cast<Call>()->id().c_str(); 
          for(int m = 1;m<items[i]->cast<AssignI>()->e()->cast<Call>()->n_args();m++){
            int fntype = items[i]->cast<AssignI>()->e()->cast<Call>()->arg(m)->eid();
            // apply switch case again 
            if(fntype==Expression::E_ARRAYLIT){
              //number of elements in the array
              int vec_size = items[i]->cast<AssignI>()->e()->cast<Call>()->arg(m)->cast<ArrayLit>()->getVec().size();
              List ArrVec;
              for(int p = 0;p < vec_size; p++ ){
                // get the expression form of each element
                Expression *exp = items[i]->cast<AssignI>()->e()->cast<Call>()->arg(m)->cast<ArrayLit>()->getVec().operator[](p);
                if(exp->isUnboxedInt()){
                  ArrVec.push_back((double)exp->unboxedIntToIntVal().toInt());
                }else if(exp->isUnboxedFloatVal()){
                  ArrVec.push_back(exp->unboxedFloatToFloatVal().toDouble());
                }
                
              }
              thisSol.push_back(ArrVec);
            }else{
              Rcpp::stop("function call not supported");
            }
          }
          break;
        default:
          Rcpp::stop("This assignment is not supported yet");
        }
      }else {
        Rcpp::stop("Solution string contains non assignments");
        break;
      }
      if(thisSol.size())  thisSol.names() = varName;
      else Rcpp::stop("Not able to parse solution-- The value is not supported yet.");
    }
    string track_nsol = "solution:";
    track_nsol.append(to_string(nsol));
    if(nsol == solutions.size()-1 && optimal_sol_flag){
      track_nsol = "optimal_solution";
    }else if(nsol == solutions.size()-1){
      track_nsol = "best_solution";
    }
    retVal.push_back(thisSol);
    nameretVal.push_back(track_nsol);
  }
  retVal.names() = nameretVal;
  
  return retVal;
}
