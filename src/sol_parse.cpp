#include <Rcpp.h>
#include <regex>
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
    ostream& os = cerr;
    vector<SyntaxError> se;
    Model *model = MiniZinc::parseFromString(*env, solutionString, "",  ip, true, true, true, os, se);
    int s = (model-> size());
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
          if(items[i]->cast<AssignI>()->e()->cast<SetLit>()->isv()!=NULL){
            // integer set
            int max_val =  items[i]->cast<AssignI>()->e()->cast<SetLit>()->isv()->max().toInt();
            int min_val = items[i]->cast<AssignI>()->e()->cast<SetLit>()->isv()->min().toInt();  
            IntegerVector setVec = {max_val, min_val}; 
            thisSol.push_back(setVec);
          }else{
            // floating point set
            float max_val =  items[i]->cast<AssignI>()->e()->cast<SetLit>()->fsv()->max().toDouble();
            float min_val = items[i]->cast<AssignI>()->e()->cast<SetLit>()->fsv()->min().toDouble();
            NumericVector setVec = {max_val, min_val};
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
              // to be done
            }
          }
          break;
        default:;
        //cout << "invalid variable type " << endl;
        }
      }else {
        thisSol = {SOL_ERROR};
        thisSol.names() = "no_solution";
        break;
        // "not a solution string";
      }
      thisSol.names() = varName;
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
