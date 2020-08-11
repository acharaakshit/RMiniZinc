#include <Rcpp.h>
#include <minizinc/ast.hh>
#include <minizinc/hash.hh>
#include <minizinc/prettyprinter.hh>
#include "pathStringcheck.h"
#include "helper_parse.h"
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
  // to store the output item
  List output;
  
  for(int i=0; i < model->size(); i++){
    items.push_back(model->operator[] (i));
    string itemTrack = "item: ";
    itemTrack.append(to_string(i + 1));
    if(items[i]->iid() == Item::II_VD){
      List variableDetails;
      expDetails(items[i]->cast<VarDeclI>()->e(), variableDetails);
      variableDetails.push_back(i);
      variableDetails.names() = CharacterVector({"DETAILS", "ITEM_NO"});
      variables.push_back(variableDetails);
    }else if(items[i]->iid() == Item::II_CON){
      // constraint
      List constraintInfo;
      Expression *cExp = items[i]->cast<ConstraintI>()->e();
      List cstDetails;
      expDetails(cExp, cstDetails);
      constraintInfo.push_back(cstDetails);
      constraintInfo.push_back(i);
      constraintInfo.names() = CharacterVector({"DETAILS", "ITEM_NO"});
      constraints.push_back(constraintInfo);
    }else if(items[i]->iid() == Item::II_SOL){
      // satisfaction, minimization or maximization problem
      SolveI *ci = items[i]->cast<SolveI>();
      string objectiv;
      CharacterVector objnms;
      List slvDetails;
      CharacterVector slvnms;
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
          List slvExp;
          expDetails(ci->e(), slvExp);
          slvDetails.push_back(slvExp);
          slvnms.push_back("EXPRESSION");
        }catch(std::exception &e){
          Rcpp::stop(e.what());
        }
      }  
      if(!ci->ann().isEmpty()){
        List slvAnns;
        CharacterVector anms;
        int annCount = 0;
        for(ExpressionSetIter si = ci->ann().begin(); si != ci->ann().end(); ++si){
          List slvAnn;
          Expression *e = *si;
          expDetails(e, slvAnn);
          slvAnns.push_back(slvAnn);
          string ann = "ARG";
          ann.append(to_string(annCount+1));
          anms.push_back(ann);
          annCount++;
        }
        slvAnns.names() = anms;
        slvDetails.push_back(slvAnns);
        slvnms.push_back("ANNOTATION");
     }
      slvDetails.names() = slvnms;
      objective.push_back(objectiv);
      objnms.push_back("OBJECTIVE");
      objective.push_back(i);
      objnms.push_back("ITEM_NO");
      if(slvDetails.length()){
        objective.push_back(slvDetails);
        objnms.push_back("DETAILS");
      }
      objective.names() = objnms;
    }else if(items[i]->iid() == Item::II_FUN ){
      // function
      List fnDetails;
      FunctionI *fi = items[i]->cast<FunctionI>();
      fnDetails.push_back(fi->id().c_str());
      List fnDets;
      CharacterVector fnDetnms;
      
      if(fi->e() != NULL){
        List fnExp;
        expDetails(fi->e(), fnExp);
        fnDets.push_back(fnExp);
        fnDetnms.push_back("EXPRESSION");
      }else{
        Rcpp::warning ("Detected function without expression");
      }
      
      // Return type of the function
      if(fi->ti()->type().isbool() && fi->ti()->type().ispar()){
        fnDets.push_back("test");
        fnDetnms.push_back("FUNCTION_PREFIX");
      }else if(fi->ti()->type().isbool() && fi->ti()->type().isvar()){
        fnDets.push_back("predicate");
        fnDetnms.push_back("FUNCTION_PREFIX");
      }
    
      if(!fi->ann().isEmpty()){
        List fnAnns;
        CharacterVector anms;
        int annCount = 0;
        for(ExpressionSetIter si = fi->ann().begin(); si != fi->ann().end(); ++si){
          List fnAnn;
          Expression *e = *si;
          expDetails(e, fnAnn);
          fnAnns.push_back(fnAnn);
          string ann = "ARG";
          ann.append(to_string(annCount+1));
          anms.push_back(ann);
          annCount++;
        }
        fnAnns.names() = anms;
        fnDets.push_back(fnAnns);
        fnDetnms.push_back("ANNOTATION");
      }
      
      List fnParLists;
      CharacterVector fpnms;
      ASTExprVec<VarDecl> pars = fi->params();
      for(int k = 0; k < pars.size();k++){
        List fnParList;
        CharacterVector fplnms;
        
        VarDecl *pe = pars.operator[](k);
        fnParList.push_back(pe->id()->v().c_str());
        fplnms.push_back("NAME");
        fnParList.push_back(vType(pe->type()));
        fplnms.push_back("TYPE");
      
        Expression *dExp = pe->ti()->domain();
        if(dExp!=NULL){
          List varDomain;
          expDetails(dExp, varDomain);
          if(varDomain.length()){
            fnParList.push_back(varDomain);
            fplnms.push_back("DOMAIN");
          }
        }
        
        Expression *vExp = pe->e();
        if(vExp != NULL){
          List varVal;
          expDetails(vExp, varVal);
          fnParList.push_back(varVal);
          fplnms.push_back("VALUE");
        }
        fnParList.names() = fplnms;
        fnParLists.push_back(fnParList);
        
        string fp = "DECL";
        fp.append(to_string(k+1));
        fpnms.push_back(fp);
      }
      
      fnParLists.names() = fpnms;
      fnDets.push_back(fnParLists);
      fnDetnms.push_back("PARAMETER_DECLARATIONS");
      fnDets.names() = fnDetnms;
      fnDetails.push_back(fnDets);
      fnDetails.push_back(i);
      fnDetails.names() = CharacterVector({"FUNCTION_NAME", "DETAILS", "ITEM_NO"});
      functions.push_back(fnDetails);
    }else if(items[i]->iid() == Item::II_INC){
      // included files
      List includeItems;
      IncludeI *ii = items[i]->cast<IncludeI>();
      includeItems.push_back(ii->f().c_str());
      includeItems.push_back(i);
      includeItems.names() = CharacterVector({"INCLUDED_MZN", "ITEM_NO"});
      includes.push_back(includeItems);
    }else if(items[i]->iid() == Item::II_OUT){
      Rcpp::warning("The model includes output formatting -- remove if parsed solutions are desired");
      List oput;
      OutputI *ot  = items[i]->cast<OutputI>();
      expDetails(ot->e() , oput);
      output.push_back(oput);
      output.push_back(i);
      output.names() = CharacterVector({"DETAILS", "ITEM_NO"});
    }else if(items[i]->iid() == Item::II_ASN){
      List assignExp;
      Expression *aExp = items[i]->cast<AssignI>()->e();
      expDetails(aExp, assignExp);
      List assignDetails;
      assignDetails.push_back(items[i]->cast<AssignI>()->id().c_str());
      assignDetails.push_back(assignExp);
      assignDetails.push_back(i);
      assignDetails.names() = CharacterVector({"NAME", "VALUE", "ITEM_NO"});
      assignments.push_back(assignDetails);
    }else{
      Rcpp::warning("element not identified or supported yet");
    }
  }
  
  // list to return
  List retVal;
  // to store the names of the return Value list
  CharacterVector retValNames;
  if(variables.length() == 0){
    Rcpp::warning("No variables found!"); 
  }else{
    CharacterVector varVecNames;
    for(int i = 0; i < variables.length(); i++){
      string v = "DECL";
      v.append(to_string(i+1));
      varVecNames.push_back(v);
    }
    variables.names() = varVecNames;
    retVal.push_back(variables);
    retValNames.push_back("VARIABLES"); 
  }
  
  if(constraints.length() == 0) {
    Rcpp::warning("No constraints found!");
  }else{
    CharacterVector constraintNames; 
    // push the constraint information
    for(int i = 0; i< constraints.length();i++){
      string c = "CONSTRAINT";
      string cNo = c.append(to_string(i+1));
      constraintNames.push_back(cNo);
    }
    constraints.names() = constraintNames;
    retVal.push_back(constraints);
    retValNames.push_back("CONSTRAINTS");
  } 
  
  // push the solveType information of the problem
  if(objective.length() == 0) {
    Rcpp::warning("No solve item found"); 
  }else{
    retVal.push_back(objective);
    retValNames.push_back("SOLVE_TYPE"); 
  }
  
  // push the used functions information
  if(functions.length()){
    CharacterVector fnVecNames;
    for(int i = 0; i < functions.length(); i++){
      string f = "FUNCTION";
      string fNo = f.append(to_string(i+1));
      fnVecNames.push_back(fNo);
    }
    functions.names() = fnVecNames;
    retVal.push_back(functions);
    retValNames.push_back("FUNCTION_ITEMS");
  }
  // push the names of included mzn files
  if(includes.length()){
    CharacterVector includeItemNames;
    for(int i = 0; i < includes.length(); i++){
      string inName = "INCLUDE";
      inName.append(to_string(i+1));
      includeItemNames.push_back(inName);
      
    }
    includes.names() = includeItemNames;
    retVal.push_back(includes);
    retValNames.push_back("INCLUDES");
  }
  
  if(assignments.length()){
    CharacterVector assignmentNames;
    for(int i = 0; i< assignments.length();i++){
      string c = "ASSIGNMENT";
      string cNo = c.append(to_string(i+1));
      assignmentNames.push_back(cNo);
    }
    assignments.names() = assignmentNames;
    retVal.push_back(assignments);
    retValNames.push_back("ASSIGNMENTS");
  }
  
  if(output.length()){
    retVal.push_back(output);
    retValNames.push_back("OUTPUT_ITEM");
  }
  
  // return the string representation of the model
  stringstream strmodel;
  Printer *p = new Printer(strmodel); 
  p->print(model);
  string mString = strmodel.str();
  retVal.push_back(mString);
  retValNames.push_back("MODEL_STRING");
  retVal.names() = retValNames;
  return retVal;
}


