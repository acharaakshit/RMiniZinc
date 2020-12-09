#include "config.h"
#include "helper_parse.h"


using namespace Rcpp;

//' @title MiniZinc syntax parser
//' 
//' @description parses the MiniZinc syntax into R objects
//'
//' @importFrom Rcpp sourceCpp
//' @export mzn_parse
//' @useDynLib rminizinc, .registration=TRUE
//' @param model_string string representation of the MiniZinc model.
//' @param mzn_path the path of model mzn.
//' @param include_path path of the included mzn in the model if it exists.
// [[Rcpp::export]]
Environment mzn_parse(std::string model_string = "",
                      std::string mzn_path = "",
                      Nullable<std::vector<std::string>> include_path = R_NilValue);

#ifdef MZN_PARSE

#include <minizinc/parser.hh>
#include <minizinc/ast.hh>
#include <minizinc/hash.hh>
#include <minizinc/prettyprinter.hh>

using namespace std;
using namespace MiniZinc;


MiniZinc::Model* parse_help(std::string modelString, std::string modelStringName,
                              std::vector<std::string> includePath){
  
  for(int i = 0; i < includePath.size(); i++){
    int retIP = dirExists(includePath[i].c_str());
    if(retIP == 0){
      Rcpp::stop("Include path directory doesn't exist or not a directory");
    }else if(retIP < 0){
      Rcpp::stop("Error occured in include path");
    }
  }
  
  Env* env = new Env();
  // include paths of mzn files to be included
  vector<SyntaxError> se;
  Model *model;
  
  Rcpp::Environment utils("package:utils");
  Rcpp::Function utils_cpp = utils["data"];  
  utils_cpp("config");
  std::string mk =  Rcpp::as<string>(Environment::global_env()["LIBMINIZINC_PATH"]);
  mk.reserve(50);    
  
  size_t start = mk.find("-L");
  size_t end = mk.find("libminizinc", start);
  string sub = mk.substr(start + 2, end - start - 2);
  sub.append("libminizinc/share/minizinc/std");
  includePath.push_back(sub);
  try{
    std::stringstream ss;
    //change the underlying buffer and save the old buffer
    auto old_buf = std::cerr.rdbuf(ss.rdbuf()); 
    model = MiniZinc::parse_from_string(*env, modelString, modelStringName , includePath,
                                      false, false, false, false, Rcpp::Rcerr, se);
    std::cerr.rdbuf(old_buf); //reset
    Rcerr << ss.str();
    if(model==NULL) throw std::exception();
    else if(model->size() == 0) Rcpp::stop("Empty Model!");
  }catch(std::exception& e){
    Rcpp::stop("NULL model !");
  }
  return model;
}

// mapping of BinOp type with strings
std::string bo_str_map(BinOpType OP){
  if(OP == BinOpType::BOT_DOTDOT) return "'..'";
  else if(OP == BinOpType::BOT_MINUS) return "'-'";
  else if(OP == BinOpType::BOT_PLUS) return "'+'" ;
  else if(OP == BinOpType::BOT_MOD) return "'mod'";
  else if(OP == BinOpType::BOT_DIV) return "'/'";
  else if(OP == BinOpType::BOT_POW) return "'^'";
  else if(OP == BinOpType::BOT_MULT) return "'*'";
  else if(OP == BinOpType::BOT_EQ) return "'='";
  else if(OP == BinOpType::BOT_GQ) return "'>='";
  else if(OP == BinOpType::BOT_GR) return "'>'";
  else if(OP == BinOpType::BOT_INTERSECT) return "'intersect'";
  else if(OP == BinOpType::BOT_LE) return "'<'";
  else if(OP == BinOpType::BOT_LQ) return "'<='";
  else if(OP == BinOpType::BOT_AND) return "'/\\'";
  else if(OP == BinOpType::BOT_OR) return "'\\/'";
  else if(OP == BinOpType::BOT_IMPL) return "'->'";
  else if(OP == BinOpType::BOT_NQ) return "'!='";
  else if(OP == BinOpType::BOT_UNION) return "'union'";
  else if(OP == BinOpType::BOT_SUBSET) return "'subset'";
  else if(OP == BinOpType::BOT_SUPERSET) return "'superset'";
  else if(OP == BinOpType::BOT_DIFF) return "'diff'";
  else if(OP == BinOpType::BOT_SYMDIFF) return "'symdiff'";
  else if(OP == BinOpType::BOT_PLUSPLUS) return "'++'";
  else if(OP == BinOpType::BOT_XOR) return "'xor'";
  else if(OP == BinOpType::BOT_IN) return "'in'";
  else if(OP == BinOpType::BOT_EQUIV) return "'<->'";
  else if(OP == BinOpType::BOT_RIMPL) return "'<-'";
  else return "'div'";
}

// mapping of UnOp type with strings
std::string uo_str_map(UnOpType OP){
  if(OP == UnOpType::UOT_PLUS) return "'+'";
  else if(OP == UnOpType::UOT_MINUS) return "'-'";
  else return "'not'";
}

// Type details for variable declarations
std::string get_type(Type tp){
  if(tp.isAnn()) return ("annotation"); 
  //else if(tp.bt() == Type::BT_BOT) return("bot");
  //else if(tp.bt() == Type::BT_TOP) return("top");
  else if(tp.isint()) return ("int");
  else if(tp.isfloat()) return ("float");
  else if(tp.isbool()) return ("bool");
  else if(tp.isstring()) return ("string");
  else if(tp.isSet()){
    if(tp.bt() == Type::BT_INT) return ("set of int");
    else if(tp.bt() == Type::BT_FLOAT) return ("set of float");
    else if(tp.bt() == Type::BT_BOOL) return ("set of bool");
    else if(tp.bt() == Type::BT_STRING) return ("set of string");
    else return ("unknown set");
  }else if(tp.dim() >= 1  && !tp.isSet()){
    string arr_tp = to_string(tp.dim());
    if(tp.bt() == Type::BT_INT && tp.st() == Type::ST_SET) return arr_tp.append(" dimensional array of set of int");
    else if(tp.bt() == Type::BT_FLOAT && tp.st() == Type::ST_SET) return arr_tp.append(" dimensional array of set of float");
    else if(tp.bt() == Type::BT_BOOL && tp.st() == Type::ST_SET) return arr_tp.append(" dimensional array of set of bool");
    else if(tp.bt() == Type::BT_STRING && tp.st() == Type::ST_SET) return arr_tp.append(" dimensional array of set of string");
    else if(tp.bt() == Type::BT_INT) return arr_tp.append(" dimensional array of int");
    else if(tp.bt() == Type::BT_FLOAT) return arr_tp.append(" dimensional array of float");
    else if(tp.bt() == Type::BT_BOOL) return arr_tp.append(" dimensional array of bool");
    else if(tp.bt() == Type::BT_STRING) return arr_tp.append(" dimensional array of string");
    else return arr_tp.append(" dimensional unknown array");
  }
  return "type couldn't be  identified";
}

void exp_details(MiniZinc::Expression *exp, List &expList){
  if(exp == NULL){
    Rcpp::stop("Parse error"); 
  }if(exp->eid() == Expression::E_COMP){
    List Comp;
    List Gentrs;
    CharacterVector gtnms;
    Comprehension *cp = exp->cast<Comprehension>();
    int n_genrtrs =  cp->numberOfGenerators();
    for(int i = 0; i<n_genrtrs;i++){
      List Gentr;
      List declLists;
      List inList;
      List whereList;
      CharacterVector gtrnms;
      CharacterVector dnms;
      Expression *inExp = cp->in(i);
      Expression *whereExp = cp->where(i);
      for(int j = 0; j < cp->numberOfDecls(i); j++){
        List declList;
        Expression *vd = cp->decl(i, j);
        exp_details(vd, declList);
        declLists.push_back(declList);
        string vn = "DECL";
        vn.append(to_string(j+1));
        dnms.push_back(vn);
      }
      
      if(declLists.length()){
        declLists.names() = dnms;
        Gentr.push_back(declLists);
        gtrnms.push_back("DECLARATIONS");
      }
      
      // in expression for this generator
      if(inExp != NULL){
        exp_details(inExp, inList);
        Gentr.push_back(inList);
        gtrnms.push_back("IN");
      }
      // where expression for this generator
      if(whereExp != NULL){
        exp_details(whereExp, whereList);
        Gentr.push_back(whereList);
        gtrnms.push_back("WHERE");
      }
      
      Gentr.names() = gtrnms;
      string gt = "GENERATOR";
      gt.append(to_string(i+1));
      gtnms.push_back(gt);
      Gentrs.push_back(Gentr);
    }
    
    Gentrs.names() = gtnms;
    Comp.push_back(Gentrs);
    Comp.push_back(cp->set());
    Comp.names() = CharacterVector({"GENERATORS", "SET"}); 
    List cExp;
    if(exp->cast<Comprehension>()->e() != NULL){
      exp_details(exp->cast<Comprehension>()->e() , cExp);
      Comp.push_back(cExp);
      Comp.names() = CharacterVector({"GENERATORS", "IS_SET", "EXPRESSION"}); 
    }
    expList.push_back(Comp);
    expList.names() = CharacterVector({"COMPREHENSION"});
  }else if(exp->eid() == Expression::E_SETLIT){
    SetLit *sl = exp->cast<SetLit>();
    List setList;
    if(sl->isv()!= NULL){  
      int min_val = sl->isv()->min().toInt();
      int max_val = sl->isv()->max().toInt();
      IntegerVector setVec = {min_val, max_val}; 
      setVec.names() = CharacterVector({"IMIN", "IMAX"});
      expList.push_back(setVec);
    }else if(sl->fsv()!=NULL){
      float min_val =  sl->fsv()->min().toDouble();
      float max_val =  sl->fsv()->max().toDouble();
      NumericVector setVec = {min_val, max_val};
      setVec.names() = CharacterVector({"FMIN", "FMAX"});
      expList.push_back(setVec);
    }else{
      ASTExprVec<Expression> expVec = sl->v();
      List setVecs;
      CharacterVector stnms;
      for(int p = 0; p < expVec.size(); p++){
        List setVec;
        exp_details(expVec.operator[](p), setVec);
        setVecs.push_back(setVec);
        string st = "ELEMENT";
        st.append(to_string(p+1));
        stnms.push_back(st);
      }
      setVecs.names() = stnms;
      expList.push_back(setVecs);
    }
    expList.names() = CharacterVector({"SET"});
  }else if(exp->eid() == Expression::E_ID){
    expList.push_back(exp->cast<Id>()->str().c_str());
    expList.names() = CharacterVector({"ID"});
  }else if(exp->eid() == Expression::E_INTLIT){
    expList.push_back(exp->cast<IntLit>()->v().toInt());
    expList.names() = CharacterVector({"INT"});
  }else if(exp->eid() == Expression::E_FLOATLIT){
    expList.push_back(exp->cast<FloatLit>()->v().toDouble());
    expList.names() = CharacterVector({"FLOAT"});
  }else if(exp->eid() == Expression::E_BOOLLIT){
    expList.push_back(exp->cast<BoolLit>()->v());
    expList.names() = CharacterVector({"BOOL"});
  }else if(exp->eid() == Expression::E_STRINGLIT){
    expList.push_back(exp->cast<StringLit>()->v().c_str());
    expList.names() = CharacterVector({"STRING"});
  }else if(exp->eid() == Expression::E_ARRAYLIT){
    ArrayLit *al = exp->cast<ArrayLit>();
    List ArrVecs;
    CharacterVector arrnms;
    List AlDim;
    CharacterVector aldimnms;
    for(int i = 0; i < al->dims(); i++){
      AlDim.push_back(IntegerVector::create(Named("MIN",al->min(i)), Named("MAX")=al->max(i)));
      string d = "DIM";
      d.append(to_string(i+1));
      aldimnms.push_back(d);
    }
    AlDim.names() = aldimnms;
    for(int p = 0;p < al->getVec().size(); p++ ){
      // get the expression form of each element
      List ArrVec;
      exp_details(al->getVec().operator[](p), ArrVec);
      ArrVecs.push_back(ArrVec);
      string av = "ELEMENT";
      av.append(to_string(p+1));
      arrnms.push_back(av);
    }
    ArrVecs.names() = arrnms;
    List AV = List::create(Named("DIM_SIZE") = AlDim , _["ELEMENTS"] = ArrVecs);
    expList.push_back(AV);
    expList.names() = CharacterVector({"ARRAY"});
  }else if(exp->eid() == Expression::E_CALL){ 
    Call *cl = exp->cast<Call>();
    List cArgs;
    List cnms;
    for(int k = 0; k < cl->argCount(); k++){
      List cArg;
      exp_details(cl->arg(k), cArg);
      cArgs.push_back(cArg);
      string ct = "ARG";
      ct.append(to_string(k+1));
      cnms.push_back(ct);
    }
    cArgs.names() = cnms;
    List cList;
    cList.push_back(cl->id().c_str());
    cList.push_back(cArgs);
    cList.names() = CharacterVector({"NAME", "ARGUMENTS"});
    expList.push_back(cList);
    expList.names() = CharacterVector({"FUNCTION_CALL"});
  }else if(exp->eid() == Expression::E_BINOP){
    BinOp *boExp = exp->cast<BinOp>();
    List boLhs;
    exp_details(boExp->lhs(), boLhs);
    List boRhs;
    exp_details(boExp->rhs(), boRhs);
    List boList;
    boList.push_back(boLhs);
    boList.push_back(bo_str_map(boExp->op()));
    boList.push_back(boRhs);
    boList.names() = CharacterVector({"LHS", "BINARY_OPERATOR", "RHS"});
    expList.push_back(boList);
    expList.names() = CharacterVector({"BINARY_OPERATION"});
  }else if(exp->eid() == Expression::E_UNOP){
    UnOp *uo = exp->cast<UnOp>();
    List uoArgs;
    CharacterVector uonms;
    for(int i=0; i < uo->argCount(); i++) {
      List uoArg;
      exp_details(uo->arg(i), uoArg);
      uoArgs.push_back(uoArg);
      string ut = "ARG";
      ut.append(to_string(i+1));
      uonms.push_back(ut);
    }
    uoArgs.names() = uonms;
    List uoList;
    uoList.push_back(uo_str_map(uo->op()));
    uoList.push_back(uoArgs);
    uoList.names() = CharacterVector({"UNARY_OPERATOR", "ARGUMENTS"});
    expList.push_back(uoList);
    expList.names() = CharacterVector({"UNARY_OPERATION"});
  }else if(exp->eid() == Expression::E_LET){
    Let *lt = exp->cast<Let>();
    List inExp;
    exp_details(lt->in(), inExp); 
    List ltExps;
    CharacterVector ltnms;
    ASTExprVec<Expression> letVec = lt->let();
    for(int i = 0; i< letVec.size(); i++ ){
      List ltExp;
      exp_details(letVec.operator[](i), ltExp);
      ltExps.push_back(ltExp);
      string lt = "LET";
      lt.append(to_string(i+1));
      ltnms.push_back(lt);
    }
    ltExps.names() = ltnms;
    List letList;
    letList.push_back(ltExps); letList.push_back(inExp);
    letList.names() = CharacterVector({"LET_EXPRESSION", "IN_EXPRESSION"});
    expList.push_back(letList);
    expList.names() = CharacterVector({"LET"});
  }else if(exp->eid() == Expression::E_ARRAYACCESS){
    List aaDetails;
    exp_details(exp->cast<ArrayAccess>()->v(), aaDetails);
    const ASTExprVec<Expression> astExp = exp->cast<ArrayAccess>()->idx();
    List aaArgs;
    CharacterVector aanms;
    for(int i = 0; i < astExp.size(); i++){
      List aaArg;
      exp_details(astExp.operator[](i), aaArg);
      aaArgs.push_back(aaArg);
      string aa = "ARG";
      aa.append(to_string(i+1));
      aanms.push_back(aa);
    }
    aaArgs.names() = aanms;
    List aaList;
    aaList.push_back(aaDetails);
    aaList.push_back(aaArgs);
    aaList.names() = CharacterVector({"NAME", "ARGUMENTS"});
    expList.push_back(aaList);
    expList.names() = CharacterVector({"ARRAY_ACCESS"});
  }else if(exp->eid() == Expression::E_ITE){
    ITE *ite = exp->cast<ITE>();
    List ifExps;
    CharacterVector ifnms;
    List thenExps;
    CharacterVector thennms;
    for(int i=0; i < ite->size(); i++){
      List ifExp;
      exp_details(ite->ifExpr(i), ifExp);
      ifExps.push_back(ifExp);
      string iF = "IF";
      iF.append(to_string(i+1));
      ifnms.push_back(iF);
      
      List thenExp;
      exp_details(ite->thenExpr(i), thenExp);
      thenExps.push_back(thenExp);
      string then = "THEN";
      then.append(to_string(i+1));
      thennms.push_back(then);
    }
    ifExps.names() = ifnms;
    thenExps.names() = thennms;
    List elseExp;
    exp_details(ite->elseExpr(), elseExp);
    List iteList = {ifExps, thenExps, elseExp};
    iteList.names() = CharacterVector({"IF", "THEN", "ELSE"});
    expList.push_back(iteList);
    expList.names() = CharacterVector({"IF_THEN_ELSE"});
  }else if(exp->eid() == Expression::E_VARDECL){
    VarDecl *vd =  exp->cast<VarDecl>();
    List vdDetails;
    CharacterVector vdnms;
    exp_details(vd->id(), vdDetails);
    vdnms.push_back("NAME");
    // vdDetails.push_back(vType(vd->type()));
    // vdnms.push_back("TYPE");
    exp_details(vd->ti(), vdDetails);
    vdnms.push_back("TYPE_INST");
    
    Expression *vExp = vd->e();
    if(vExp != NULL){
      List varVal;
      exp_details(vExp, varVal);
      vdDetails.push_back(varVal);
      vdnms.push_back("VALUE");
    }
    vdDetails.names() = vdnms;
    
    expList.push_back(vdDetails);
    expList.names() = CharacterVector({"VARIABLE_DECLARATION"});
  }else if(exp->eid() == Expression::E_TI){
    // to be supported soon
    TypeInst *tExp = exp->cast<TypeInst>();
    List TI;
    CharacterVector tinms;
    
    // decision variables or parameters
    if(tExp->type().isPar()){
      //parameter
      TI.push_back("par");
    }else{
      //decision variables
      TI.push_back("var");
    }
    tinms.push_back("KIND");
    
    Expression *dExp = tExp->domain();
    if(dExp!=NULL){
      List varDomain;
      exp_details(dExp, varDomain);
      if(varDomain.length()){
        TI.push_back(varDomain);
        tinms.push_back("DOMAIN");
      }
    }
    
    List indices;
    CharacterVector indnms;
    ASTExprVec<TypeInst> index_ti = tExp->ranges();
    for(int s = 0; s < index_ti.size(); s++){
      List index;
      if(index_ti.operator[](s)->domain() != NULL){
        exp_details(index_ti.operator[](s)->domain() , index);
        indices.push_back(index);
      }else{
        index.push_back("int");
        index.names() = CharacterVector({"UNRESTRICTED"});
        indices.push_back(index);
      }
      string ix = "INDEX";
      ix.append(to_string(s+1));
      indnms.push_back(ix);
    }
    
    if(indices.length()){
      indices.names() = indnms;
      TI.push_back(indices);
      tinms.push_back("INDEX");
    }
    
    TI.push_back(get_type(tExp->type()));
    tinms.push_back("TYPE");
    
    TI.names() = tinms;
    expList.push_back(TI);
    expList.names() = CharacterVector({"TYPE_INST"});
  }else{
    if(expList.length())
      Rcpp::warning("Expression couldn't be parsed entirely");
    else
      Rcpp::warning("Expression couldn't be parsed");
  }
}

Environment mzn_parse(std::string model_string,
                      std::string mzn_path,
                      Nullable<std::vector<std::string>> include_path){
  
  model_string  = pathStringcheck(model_string, mzn_path);
  vector<string> ip;
  if(!Rf_isNull(include_path)){
    ip = Rcpp::as<vector<string>>(include_path);
  }
  
  Model *model = parse_help(model_string, "mzn_parse.mzn", ip);  
  
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
      exp_details(items[i]->cast<VarDeclI>()->e(), variableDetails);
      variableDetails.push_back(i);
      variableDetails.names() = CharacterVector({"DETAILS", "ITEM_NO"});
      variables.push_back(variableDetails);
    }else if(items[i]->iid() == Item::II_CON){
      // constraint
      List constraintInfo;
      Expression *cExp = items[i]->cast<ConstraintI>()->e();
      List cstDetails;
      exp_details(cExp, cstDetails);
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
          exp_details(ci->e(), slvExp);
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
          exp_details(e, slvAnn);
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
        exp_details(fi->e(), fnExp);
        fnDets.push_back(fnExp);
        fnDetnms.push_back("EXPRESSION");
      }else{
        Rcpp::warning ("Detected function without expression");
      }
      // return the return type-inst information
      exp_details(fi->ti(), fnDets);
      fnDetnms.push_back("TYPE_INST");
      
      if(!fi->ann().isEmpty()){
        List fnAnns;
        CharacterVector anms;
        int annCount = 0;
        for(ExpressionSetIter si = fi->ann().begin(); si != fi->ann().end(); ++si){
          List fnAnn;
          Expression *e = *si;
          exp_details(e, fnAnn);
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
        CharacterVector fplnms;
        
        Expression *pd = pars.operator[](k);
        exp_details(pd, fnParLists);
        string fp = "DECL";
        fp.append(to_string(k+1));
        fpnms.push_back(fp);
      }
      fnParLists.names() = fpnms;
      fnDets.push_back(fnParLists);
      fnDetnms.push_back("DECLARATIONS");
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
      exp_details(ot->e() , oput);
      output.push_back(oput);
      output.push_back(i);
      output.names() = CharacterVector({"DETAILS", "ITEM_NO"});
    }else if(items[i]->iid() == Item::II_ASN){
      List assignExp;
      Expression *aExp = items[i]->cast<AssignI>()->e();
      exp_details(aExp, assignExp);
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
    Rcpp::warning("No solve item found!"); 
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
  Function asNamespace("asNamespace");
  Environment rmzn_env = asNamespace("rminizinc");
  Function retModel = rmzn_env["getRModel"];
  Environment ret_env = retModel(retVal);
  return ret_env;
}

#else

Environment mzn_parse(std::string model_string,
                      std::string mzn_path,
                      Nullable<std::vector<std::string>> include_path){
  Rcpp::stop("Please install libminizinc on your system!");
}

#endif