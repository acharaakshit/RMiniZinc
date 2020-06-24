#include <Rcpp.h>
#include <minizinc/prettyprinter.hh>
#include "filetoString.h"
#include "helper_parse.h"
#include "expVarNames.h"

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
//' @param mznpath the path of model mzn.
//' @param modelStringName the name of model string.
// [[Rcpp::export]]
List mzn_parse(std::string modelString = "", 
                      std::string mznpath = "",
                      std::string  modelStringName = "mzn_parse.mzn"){
  
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

  Model *model = helper_parse(modelString, modelStringName);  
  
  List retVal;
  // size of the model
  int s = model-> size();
  if(s == 0){
    Rcpp::stop("Empty model!");
  }
  // get all the items and the names of all the parameters and map to the item numbers
  vector<Item*> items;
  int type = 0;
  // to store the missing parameter names
  CharacterVector parameters;
  // to store the decision variable names
  CharacterVector decisionVars;
  // to store the variables involved in the constraints
  List constraints;
  List constraintVars;
  int constraintCount = 0;
  // to store the solvetype of the problem
  List objective;
  for(int i=0; i < s; i++){
    items.push_back(model->operator[] (i));
    if(items[i]->iid() == Item::II_VD){
      string name = items[i]->cast<VarDeclI>()->e()->id()->str().aststr()->c_str();
      // decision variables or parameters
     if(items[i]->cast<VarDeclI>()->e()->type().ispar()){
        //parameter
        parameters.push_back(name);
     }else if(items[i]->cast<VarDeclI>()->e()->type().isvar()){
       //decision variables
       decisionVars.push_back(name);
     }
    }else if(items[i]->iid() == Item::II_CON){
      // constraint
      Expression *cExp = items[i]->cast<ConstraintI>()->e();
      vector<string> cstNames;
      expVarNames(cExp, cstNames);
      constraintVars.push_back(cstNames);
      constraintCount++;
    }else if(items[i]->iid() == Item::II_SOL){
      // satisfaction, minimization or maximization problem
      SolveI *ci = items[i]->cast<SolveI>();
      string objectiv;
      vector<string> cstNames;
      Expression *optimizeExp =  ci->e();
      if(ci->st() == SolveI::ST_SAT){
        objectiv = "satisfy";
      }else {
        if(ci->st() == SolveI::ST_MIN){
          objectiv = "miniminze";
        }else{
          objectiv = "maximize";
        }
        try{
          expVarNames(ci->e(), cstNames);
        }catch(std::exception &e){
          Rcpp::stop(e.what());
        }
      }
      objective.push_back(objectiv);
      objective.names() = CharacterVector({"SolveType"});
      if(cstNames.size()){
        objective.push_back(cstNames); 
        objective.names() = CharacterVector({"SolveType", "VarsIncluded"});
      }
        
    }
  }
  // push the missing parameter names  
  retVal.push_back(parameters);
  //push the decision variable names
  retVal.push_back(decisionVars);
  // push the constraint information
  constraints.push_back(constraintCount);
  // name the constraint vars based on the number of constraints
  CharacterVector constraintNoTrack;
  for(int i = 0; i< constraintCount;i++){
    string c = "constraint: ";
    string cNo = c.append(to_string(i));
    constraintNoTrack.push_back(cNo);
  }
  constraintVars.names() = constraintNoTrack;
  constraints.push_back(constraintVars);
  constraints.names() = CharacterVector({"noOfConstraints", "varsInvolved"});
  retVal.push_back(constraints);
  // push the objective of the problem
  retVal.push_back(objective);
  // return the string representation of the model
  stringstream strmodel;
  Printer *p = new Printer(strmodel); 
  p->print(model);
  string mString = strmodel.str();
  retVal.push_back(mString);
  retVal.names() = CharacterVector({"Parameters","decisionVariables",
                      "Constraints", "SolveType", "modelString"});
  return retVal;
}


