#include <Rcpp.h>
#include <minizinc/ast.hh>
#include <minizinc/prettyprinter.hh>
#include "pathStringcheck.h"
#include "helper_parse.h"
#include "helper_sol_parse.h"
#include "expVarNames.h"

using namespace Rcpp;
using namespace std;
using namespace MiniZinc;


// mapping of BinOp type with strings
std::string boStrMap(BinOpType OP){
  if(OP == BinOpType::BOT_DOTDOT) return "DOTDOT";
  else if(OP == BinOpType::BOT_MINUS) return "MINUS";
  else if(OP == BinOpType::BOT_PLUS) return "PLUS" ;
  else if(OP == BinOpType::BOT_MOD) return "MOD";
  else if(OP == BinOpType::BOT_POW) return "RAISE_TO";
  else if(OP == BinOpType::BOT_MULT) return "MULTIPLY";
  else return "not added currently";
}

// helper function to parse domain
void parseDomain(Expression *dExp, List &varDomain){
    if(dExp->eid() == Expression::E_SETLIT){
      helper_sol_parse(dExp, varDomain);
      varDomain.names() = CharacterVector({"Set"});
    }else if(dExp->eid() == Expression::E_ID){
      varDomain.push_back(dExp->cast<Id>()->str().c_str());
    }else if(dExp->eid() == Expression::E_INTLIT){
      varDomain.push_back(dExp->cast<IntLit>()->v().toInt());
    }else if(dExp->eid() == Expression::E_FLOATLIT){
      varDomain.push_back(dExp->cast<FloatLit>()->v().toDouble());
    }else if(dExp->eid() == Expression::E_CALL){
      Call *cl = dExp->cast<Call>();
      List cList;
      cList.push_back(cl->id().str().c_str());
      List cArgs;
      for(int k = 0; k < cl->n_args(); k++){
        parseDomain(cl->arg(k), cArgs);
      }
      varDomain.push_back(cList);
      varDomain.push_back(cArgs);
      varDomain.names() = CharacterVector({"FunctionCall", "Arguments"});
    }else if(dExp->eid() == Expression::E_BINOP){
      BinOp *boExp = dExp->cast<BinOp>();
      List boLhs;
      parseDomain(boExp->lhs(), boLhs);
      varDomain.push_back(boLhs);
      varDomain.push_back(boStrMap(boExp->op()));
      List boRhs;
      parseDomain(boExp->rhs(), boRhs);
      varDomain.push_back(boRhs);
      varDomain.names() = CharacterVector({"LHS", "BINARY_OPERATOR", "RHS"});
    }else{
      if(varDomain.length())
        Rcpp::warning("Part of domain not parsed/supported currently!");
      else
        Rcpp::warning("Domain not parsed/supported currently");
    }
}


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
  int constraintCount = 0;
  
  // to store the solvetype of the problem
  List objective;
  // to store the names of included mzn files
  List includes;
 
  // to store the information about functions used 
  List functions;
  
  int fnCount = 0;
  
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
      
      if(tp.isint()) varType = ("int");
      else if(tp.isfloat()) varType = ("float");
      else if(tp.isbool()) varType = ("bool");
      else if(tp.is_set()){
        if(tp.bt() == Type::BT_INT) varType = ("set of int");
        else if(tp.bt() == Type::BT_FLOAT) varType = ("set of float");
        else if(tp.bt() == Type::BT_BOOL) varType = ("set of bool");
        else if(tp.bt() == Type::BT_STRING) varType = ("set of string");
        else varType = ("unknown set");
      }else if(tp.dim() >= 1  && !tp.is_set()){
        string arr_tp = to_string(tp.dim());
        if(tp.bt() == Type::BT_INT) arr_tp.append(" dimensional array of int");
        else if(tp.bt() == Type::BT_FLOAT) arr_tp.append(" dimensional array of float");
        else if(tp.bt() == Type::BT_BOOL) arr_tp.append(" dimensional array of bool");
        else if(tp.bt() == Type::BT_STRING) arr_tp.append(" dimensional array of string");
        else arr_tp.append(" dimensional unknown array");
        varType = (arr_tp);
      }
  
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
     variableDetails.names() = CharacterVector({"itemNo","kind", "name", "type"});
     
     Expression *dExp  = items[i]->cast<VarDeclI>()->e()->ti()->domain();
     
     if(dExp!=NULL){
       List varDomain;
       parseDomain(dExp, varDomain);
       if(varDomain.length()){
          variableDetails.push_back(varDomain);
          variableDetails.names() = CharacterVector({"itemNo","kind", "name", "type", "domain"});
       }
     }
     variables.push_back(variableDetails);
    }else if(items[i]->iid() == Item::II_CON){
      // constraint
      List constraintDetails;
      Expression *cExp = items[i]->cast<ConstraintI>()->e();
      vector<string> cstNames;
      expVarNames(cExp, cstNames);
      constraintDetails.push_back(cstNames);
      constraintDetails.push_back(i);
      constraintDetails.names() = CharacterVector({"varsInvolved", "itemNo"});
      constraints.push_back(constraintDetails);
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
          objectiv = "minimize";
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
      objective.push_back(i);
      objective.names() = CharacterVector({"objective", "itemNo"});
      if(cstNames.size()){
        objective.push_back(cstNames); 
        objective.names() = CharacterVector({"objective", "itemNo", "varsInvolved"});
      }
        
    }else if(items[i]->iid() == Item::II_FUN ){
      // function
      List fnDetails;
      FunctionI *fi = items[i]->cast<FunctionI>();
      fnDetails.push_back(fi->id().c_str());
      vector<string> nmes;
      expVarNames(fi->e(), nmes);
      fnDetails.push_back(nmes);
      fnDetails.push_back(i);
      fnDetails.names() = CharacterVector({"fnName", "varsInvolved", "itemNo"});
      functions.push_back(fnDetails);
      fnCount++;
    }else if(items[i]->iid() == Item::II_INC){
      // included files
      List includeItems;
      IncludeI *ii = items[i]->cast<IncludeI>();
      includeItems.push_back(ii->f().c_str());
      includeItems.push_back(i);
      includeItems.names() = CharacterVector({"IncludedMZN", "itemNo"});
      includes.push_back(includeItems);
    }else if(items[i]->iid() == Item::II_OUT){
      Rcpp::warning("The model includes output formatting -- make sure it is correct MiniZinc syntax");
    }else{
      //cout << "element not identified or supported yet";
    }
  }
  // to store the names of the return Value list
  CharacterVector retValNames;
  if(variables.length() == 0) Rcpp::warning("No variables found!");
  CharacterVector varVecNames;
  for(int i = 0; i < variables.length(); i++){
    string v = "decl";
    v.append(to_string(i+1));
    varVecNames.push_back(v);
  }
  variables.names() = varVecNames;
  retVal.push_back(variables);
  retValNames.push_back("Variables");
  
  if(constraintCount == 0)   Rcpp::warning("No constraints found!");
  CharacterVector constraintNames; 
  // push the constraint information
  for(int i = 0; i< constraintCount;i++){
    string c = "constraint";
    string cNo = c.append(to_string(i+1));
    constraintNames.push_back(cNo);
  }
  constraints.names() = constraintNames;
  retVal.push_back(constraints);
  retValNames.push_back("Constraints");
  
  // push the solveType information of the problem
  if(objective.length() == 0) Rcpp::warning("No solve item found");
  retVal.push_back(objective);
  retValNames.push_back("SolveType");
  
  // push the used functions information
  if(functions.length()){
    CharacterVector fnVecNames;
    for(int i = 0; i < fnCount; i++){
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


