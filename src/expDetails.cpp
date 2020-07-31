#include <Rcpp.h>
#include "expDetails.h"

using namespace Rcpp;
using namespace MiniZinc;
using namespace std;

// mapping of BinOp type with strings
std::string boStrMap(BinOpType OP){
  if(OP == BinOpType::BOT_DOTDOT) return "DOTDOT";
  else if(OP == BinOpType::BOT_MINUS) return "MINUS";
  else if(OP == BinOpType::BOT_PLUS) return "PLUS" ;
  else if(OP == BinOpType::BOT_MOD) return "MOD";
  else if(OP == BinOpType::BOT_DIV) return "DIVIDE";
  else if(OP == BinOpType::BOT_POW) return "RAISE_TO";
  else if(OP == BinOpType::BOT_MULT) return "MULTIPLY";
  else if(OP == BinOpType::BOT_EQ) return "EQUAL_TO";
  else if(OP == BinOpType::BOT_GQ) return "GREATER_THAN_OR_EQUAL_TO";
  else if(OP == BinOpType::BOT_GR) return "GREATER_THAN";
  else if(OP == BinOpType::BOT_INTERSECT) return "INTERSECTION";
  else if(OP == BinOpType::BOT_LE) return "LESS_THAN";
  else if(OP == BinOpType::BOT_LQ) return "LESS_THAN_OR_EQUAL_TO";
  else if(OP == BinOpType::BOT_AND) return "AND";
  else if(OP == BinOpType::BOT_OR) return "OR";
  else if(OP == BinOpType::BOT_IMPL) return "IMPLIES";
  else if(OP == BinOpType::BOT_NQ) return "NOT_EQUALS";
  else if(OP == BinOpType::BOT_UNION) return "UNION";
  else if(OP == BinOpType::BOT_SUBSET) return "SUBSET";
  else if(OP == BinOpType::BOT_SUPERSET) return "SUPERSET";
  else if(OP == BinOpType::BOT_DIFF) return "DIFF";
  else if(OP == BinOpType::BOT_SYMDIFF) return "SYMDIFF";
  else if(OP == BinOpType::BOT_PLUSPLUS) return "PLUS_PLUS";
  else if(OP == BinOpType::BOT_XOR) return "XOR";
  else if(OP == BinOpType::BOT_IN) return "IN";
  else if(OP == BinOpType::BOT_EQUIV) return "EQUIVALENCE";
  else if(OP == BinOpType::BOT_RIMPL) return "REVERSE_IMPLIES";
  else return "IDIV";
}

// mapping of UnOp type with strings
std::string uoStrMap(UnOpType OP){
  if(OP == UnOpType::UOT_PLUS) return "PLUS";
  else if(OP == UnOpType::UOT_MINUS) return "MINUS";
  else return "NOT";
}

// Type details for variable declarations
std::string vType(Type tp){
  if(tp.isint()) return ("int");
  else if(tp.isfloat()) return ("float");
  else if(tp.isbool()) return ("bool");
  else if(tp.is_set()){
    if(tp.bt() == Type::BT_INT) return ("set of int");
    else if(tp.bt() == Type::BT_FLOAT) return ("set of float");
    else if(tp.bt() == Type::BT_BOOL) return ("set of bool");
    else if(tp.bt() == Type::BT_STRING) return ("set of string");
    else return ("unknown set");
  }else if(tp.dim() >= 1  && !tp.is_set()){
    string arr_tp = to_string(tp.dim());
    if(tp.bt() == Type::BT_INT) arr_tp.append(" dimensional array of int");
    else if(tp.bt() == Type::BT_FLOAT) arr_tp.append(" dimensional array of float");
    else if(tp.bt() == Type::BT_BOOL) arr_tp.append(" dimensional array of bool");
    else if(tp.bt() == Type::BT_STRING) arr_tp.append(" dimensional array of string");
    else arr_tp.append(" dimensional unknown array");
    return (arr_tp);
  }
  return "type couldn't be  identified";
}

void expDetails(MiniZinc::Expression *exp, List &expList){
  if(exp->eid() == Expression::E_COMP){
    List Comp;
    List Gentrs;
    CharacterVector gtnms;
    int n_genrtrs =  exp->cast<Comprehension>()->n_generators();
    
    for(int i = 0; i<n_genrtrs;i++){
        List Gentr;
        List declLists;
        List inList;
        List whereList;
        CharacterVector gtrnms;
        CharacterVector dnms;
        Expression *inExp = exp->cast<Comprehension>()->in(i);
        Expression *whereExp = exp->cast<Comprehension>()->where(i);
        
        for(int j = 0; j < exp->cast<Comprehension>()->n_decls(i); j++){
          List declList;
          Expression *vd = exp->cast<Comprehension>()->decl(i, j);
          expDetails(vd, declList);
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
          expDetails(exp->cast<Comprehension>()->in(i), inList);
          Gentr.push_back(inList);
          gtrnms.push_back("IN");
        }
        // where expression for this generator
        if(whereExp != NULL){
          expDetails(exp->cast<Comprehension>()->where(i), whereList);
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
    Comp.names() = CharacterVector({"GENERATORS"}); 
    List cExp;
    if(exp->cast<Comprehension>()->e() != NULL){
      expDetails(exp->cast<Comprehension>()->e() , cExp);
      Comp.push_back(cExp);
      Comp.names() = CharacterVector({"GENERATORS", "EXPRESSION"}); 
    }
    expList.push_back(Comp);
    expList.names() = CharacterVector({"COMPREHENSION"});
  }else if(exp->eid() == Expression::E_SETLIT){
    SetLit *sl = exp->cast<SetLit>();
    List setList;
    if(sl->isv()!= NULL){  
      int max_val = sl->isv()->max().toInt();
      int min_val = sl->isv()->min().toInt();  
      IntegerVector setVec = {max_val, min_val}; 
      setVec.names() = CharacterVector({"MAX", "MIN"});
      expList.push_back(setVec);
    }else if(sl->fsv()!=NULL){
      float max_val =  sl->fsv()->max().toDouble();
      float min_val =  sl->fsv()->min().toDouble();
      NumericVector setVec = {max_val, min_val};
      setVec.names() = CharacterVector({"MAX", "MIN"});
      expList.push_back(setVec);
    }else{
      ASTExprVec<Expression> expVec = sl->v();
      List setVecs;
      CharacterVector stnms;
      for(int p = 0; p < expVec.size(); p++){
        List setVec;
        expDetails(expVec.operator[](p), setVec);
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
    for(int p = 0;p < al->getVec().size(); p++ ){
      // get the expression form of each element
      List ArrVec;
      expDetails(al->getVec().operator[](p), ArrVec);
      ArrVecs.push_back(ArrVec);
      string av = "ELEMENT";
      av.append(to_string(p+1));
      arrnms.push_back(av);
    }
    ArrVecs.names() = arrnms;
    expList.push_back(ArrVecs);
    expList.names() = CharacterVector({"ARRAY"});
  }else if(exp->eid() == Expression::E_CALL){ 
    Call *cl = exp->cast<Call>();
    List cArgs;
    List cnms;
    for(int k = 0; k < cl->n_args(); k++){
      List cArg;
      expDetails(cl->arg(k), cArg);
      cArgs.push_back(cArg);
      string ct = "ARG";
      ct.append(to_string(k+1));
      cnms.push_back(ct);
    }
    cArgs.names() = cnms;
    List cList;
    cList.push_back(cl->id().str().c_str());
    cList.push_back(cArgs);
    cList.names() = CharacterVector({"NAME", "ARGUMENTS"});
    expList.push_back(cList);
    expList.names() = CharacterVector({"FUNCTION_CALL"});
  }else if(exp->eid() == Expression::E_BINOP){
    BinOp *boExp = exp->cast<BinOp>();
    List boLhs;
    expDetails(boExp->lhs(), boLhs);
    List boRhs;
    expDetails(boExp->rhs(), boRhs);
    List boList;
    boList.push_back(boLhs);
    boList.push_back(boStrMap(boExp->op()));
    boList.push_back(boRhs);
    boList.names() = CharacterVector({"LHS", "BINARY_OPERATOR", "RHS"});
    expList.push_back(boList);
    expList.names() = CharacterVector({"BINARY_OPERATION"});
  }else if(exp->eid() == Expression::E_UNOP){
    UnOp *uo = exp->cast<UnOp>();
    List uoArgs;
    CharacterVector uonms;
    for(int i=0; i < uo->n_args(); i++) {
      List uoArg;
      expDetails(uo->arg(i), uoArg);
      uoArgs.push_back(uoArg);
      string ut = "ARG";
      ut.append(to_string(i+1));
      uonms.push_back(ut);
    }
    uoArgs.names() = uonms;
    List uoList;
    uoList.push_back(uoStrMap(uo->op()));
    uoList.push_back(uoArgs);
    uoList.names() = CharacterVector({"UNARY_OPERATOR", "ARGUMENTS"});
    expList.push_back(uoList);
    expList.names() = CharacterVector({"UNARY_OPERATION"});
  }else if(exp->eid() == Expression::E_LET){
    Let *lt = exp->cast<Let>();
    List inExp;
    expDetails(lt->in(), inExp); 
    List ltExps;
    CharacterVector ltnms;
    ASTExprVec<Expression> letVec = lt->let();
    for(int i = 0; i< letVec.size(); i++ ){
      List ltExp;
      expDetails(letVec.operator[](i), ltExp);
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
    expDetails(exp->cast<ArrayAccess>()->v(), aaDetails);
    const ASTExprVec<Expression> astExp = exp->cast<ArrayAccess>()->idx();
    List aaArgs;
    CharacterVector aanms;
    for(int i = 0; i < astExp.size(); i++){
      List aaArg;
      expDetails(astExp.operator[](i), aaArg);
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
      expDetails(ite->e_if(i), ifExp);
      ifExps.push_back(ifExp);
      string iF = "IF";
      iF.append(to_string(i+1));
      ifnms.push_back(iF);
      
      List thenExp;
      expDetails(ite->e_then(i), thenExp);
      thenExps.push_back(thenExp);
      string then = "THEN";
      then.append(to_string(i+1));
      thennms.push_back(then);
    }
    ifExps.names() = ifnms;
    thenExps.names() = thennms;
    List elseExp;
    expDetails(ite->e_else(), elseExp);
    List iteList = {ifExps, thenExps, elseExp};
    iteList.names() = CharacterVector({"IF", "THEN", "ELSE"});
    expList.push_back(iteList);
    expList.names() = CharacterVector({"IF_THEN_ELSE"});
  }else if(exp->eid() == Expression::E_VARDECL){
    VarDecl *vd =  exp->cast<VarDecl>();
    List vdDetails;
    CharacterVector vdnms;
    
    expDetails(vd->id(), vdDetails);
    vdnms.push_back("NAME");
    
    // decision variables or parameters
    if(vd->type().ispar()){
      //parameter
      vdDetails.push_back("PARAMETER");
    }else if(vd->type().isvar()){
      //decision variables
      vdDetails.push_back("DECISION_VARIABLE");
    }else{
      vdDetails.push_back("OTHER");
    }
    vdnms.push_back("KIND");
    
    vdDetails.push_back(vType(vd->type()));
    vdnms.push_back("TYPE");
    
    Expression *dExp = vd->ti()->domain();
    if(dExp!=NULL){
      List varDomain;
      expDetails(dExp, varDomain);
      if(varDomain.length()){
        vdDetails.push_back(varDomain);
        vdnms.push_back("DOMAIN");
      }
    }
    
    Expression *vExp = vd->e();
    if(vExp != NULL){
      List varVal;
      expDetails(vExp, varVal);
      vdDetails.push_back(varVal);
      vdnms.push_back("VALUE");
    }
    vdDetails.names() = vdnms;
    
    expList.push_back(vdDetails);
    expList.names() = CharacterVector({"VARIABLE_DECLARATION"});
  }else{
    if(expList.length())
      Rcpp::warning("Expression couldn't be parsed entirely");
    else
      Rcpp::warning("Expression couldn't be parsed");
  }
}