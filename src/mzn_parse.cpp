#include <Rcpp.h>
#include <minizinc/ast.hh>
#include <minizinc/prettyprinter.hh>
#include "pathStringcheck.h"
#include "helper_parse.h"
#include "helper_sol_parse.h"
#include "expDetails.h"

using namespace Rcpp;
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
  
  modelString  = pathStringcheck(modelString, mznpath);
  Model *model = helper_parse(modelString, modelStringName);
  
  
  List retVal;

  // get all the items and the names of all the parameters and map to the item numbers
  vector<Item*> items;
  int type = 0;
  
  // to store parameter and decision variable details
  List variables;
  
  // to store the variables involved in the constraints
  List constraints;
 
  // to store the solvetype of the problem
  List objective;
  // to store the names of included mzn files
  List includes;
 
  // to store the information about functions used 
  List functions;
  
  // to store the Assignments
  List assignments;
  
  for(int i=0; i < model->size(); i++){
    items.push_back(model->operator[] (i));
    string itemTrack = "item: ";
    itemTrack.append(to_string(i + 1));
    if(items[i]->iid() == Item::II_VD){
      List variableDetails;
      string varName;
      string varType;
      string varKind; // whether it's a variable or parameter 
      NumericVector domain;
      
      varName = items[i]->cast<VarDeclI>()->e()->id()->str().aststr()->c_str();
      
      // gather the type details 
      Type tp = items[i]->cast<VarDeclI>()->e()->type();
      varType = vType(tp);
  
     // decision variables or parameters
     if(items[i]->cast<VarDeclI>()->e()->type().ispar()){
        //parameter
        varKind = "parameter";
     }else if(items[i]->cast<VarDeclI>()->e()->type().isvar()){
        //decision variables
        varKind = "decision variable";
     }
     
     // push the variable details
     variableDetails.push_back(i);
     variableDetails.push_back(varKind);
     variableDetails.push_back(varName);
     variableDetails.push_back(varType);
     CharacterVector vDetNames = CharacterVector({"itemNo","kind", "name", "type"});;
     
     Expression *dExp  = items[i]->cast<VarDeclI>()->e()->ti()->domain();
     
     if(dExp!=NULL){
       List varDomain;
       expDetails(dExp, varDomain);
       if(varDomain.length()){
          variableDetails.push_back(varDomain);
          vDetNames.push_back("domain");
       }
     }
     
     Expression *vExp = items[i]->cast<VarDeclI>()->e()->e();
     if(vExp != NULL){
       List varVal;
       expDetails(vExp, varVal);
       variableDetails.push_back(varVal);
       vDetNames.push_back("value");
     }
     variableDetails.names() = vDetNames;
     variables.push_back(variableDetails);
    }else if(items[i]->iid() == Item::II_CON){
      // constraint
      List constraintInfo;
      Expression *cExp = items[i]->cast<ConstraintI>()->e();
      List cstDetails;
      expDetails(cExp, cstDetails);
      constraintInfo.push_back(cstDetails);
      constraintInfo.push_back(i);
      constraintInfo.names() = CharacterVector({"Details", "itemNo"});
      constraints.push_back(constraintInfo);
    }else if(items[i]->iid() == Item::II_SOL){
      // satisfaction, minimization or maximization problem
      SolveI *ci = items[i]->cast<SolveI>();
      string objectiv;
      List slvDetails;
      Expression *optimizeExp =  ci->e();
      if(ci->st() == SolveI::ST_SAT){
        objectiv = "satisfy";
      }else {
        if(ci->st() == SolveI::ST_MIN){
          objectiv = "minimize";
        }else{
          objectiv = "maximize";
        }
        try{
          expDetails(ci->e(), slvDetails);
        }catch(std::exception &e){
          Rcpp::stop(e.what());
        }
      }
      objective.push_back(objectiv);
      objective.push_back(i);
      objective.names() = CharacterVector({"objective", "itemNo"});
      if(slvDetails.size()){
        objective.push_back(slvDetails); 
        objective.names() = CharacterVector({"objective", "itemNo", "Details"});
      }
        
    }else if(items[i]->iid() == Item::II_FUN ){
      // function
      List fnDetails;
      FunctionI *fi = items[i]->cast<FunctionI>();
      fnDetails.push_back(fi->id().c_str());
      List fnDets;
      expDetails(fi->e(), fnDets);
      fnDetails.push_back(fnDets);
      fnDetails.push_back(i);
      fnDetails.names() = CharacterVector({"fnName", "Details", "itemNo"});
      functions.push_back(fnDetails);
    }else if(items[i]->iid() == Item::II_INC){
      // included files
      List includeItems;
      IncludeI *ii = items[i]->cast<IncludeI>();
      includeItems.push_back(ii->f().c_str());
      includeItems.push_back(i);
      includeItems.names() = CharacterVector({"IncludedMZN", "itemNo"});
      includes.push_back(includeItems);
    }else if(items[i]->iid() == Item::II_OUT){
      Rcpp::warning("The model includes output formatting -- remove if parsed solutions are desired");
    }else if(items[i]->iid() == Item::II_ASN){
      List assignExp;
      Expression *aExp = items[i]->cast<AssignI>()->e();
      expDetails(aExp, assignExp);
      List assignDetails;
      assignDetails.push_back(i);
      assignDetails.push_back(assignExp);
      assignDetails.names() = CharacterVector({"itemNo", "Details"});
      assignments.push_back(assignDetails);
    }else{
      Rcpp::warning("element not identified or supported yet");
    }
  }
  // to store the names of the return Value list
  CharacterVector retValNames;
  if(variables.length() == 0){
    Rcpp::warning("No variables found!"); 
  }else{
    CharacterVector varVecNames;
    for(int i = 0; i < variables.length(); i++){
      string v = "decl";
      v.append(to_string(i+1));
      varVecNames.push_back(v);
    }
    variables.names() = varVecNames;
    retVal.push_back(variables);
    retValNames.push_back("Variables"); 
  }
  
  if(constraints.length() == 0) {
    Rcpp::warning("No constraints found!");
  }else{
    CharacterVector constraintNames; 
    // push the constraint information
    for(int i = 0; i< constraints.length();i++){
      string c = "constraint";
      string cNo = c.append(to_string(i+1));
      constraintNames.push_back(cNo);
    }
    constraints.names() = constraintNames;
    retVal.push_back(constraints);
    retValNames.push_back("Constraints");
  } 
  
  // push the solveType information of the problem
  if(objective.length() == 0) {
    Rcpp::warning("No solve item found"); 
  }else{
    retVal.push_back(objective);
    retValNames.push_back("SolveType"); 
  }
  
  // push the used functions information
  if(functions.length()){
    CharacterVector fnVecNames;
    for(int i = 0; i < functions.length(); i++){
      string f = "function";
      string fNo = f.append(to_string(i+1));
      fnVecNames.push_back(fNo);
    }
    functions.names() = fnVecNames;
    retVal.push_back(functions);
    retValNames.push_back("FunctionItems");
  }
  // push the names of included mzn files
  if(includes.length()){
    CharacterVector includeItemNames;
    for(int i = 0; i < includes.length(); i++){
      string inName = "include";
      inName.append(to_string(i+1));
      includeItemNames.push_back(inName);
      
    }
    includes.names() = includeItemNames;
    retVal.push_back(includes);
    retValNames.push_back("Includes");
  }
  
  if(assignments.length()){
    CharacterVector assignmentNames;
    for(int i = 0; i< assignments.length();i++){
      string c = "assignment";
      string cNo = c.append(to_string(i+1));
      assignmentNames.push_back(cNo);
    }
    assignments.names() = assignmentNames;
    retVal.push_back(assignments);
    retValNames.push_back("Assignments");
  }
  
  // return the string representation of the model
  stringstream strmodel;
  Printer *p = new Printer(strmodel); 
  p->print(model);
  string mString = strmodel.str();
  retVal.push_back(mString);
  retValNames.push_back("modelString");
  retVal.names() = retValNames;
  return retVal;
}


