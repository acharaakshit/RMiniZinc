#include <Rcpp.h>
#include "exp_details.h"

using namespace Rcpp;
using namespace MiniZinc;
using namespace std;

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
  if(tp.isann()) return ("annotation"); 
  //else if(tp.bt() == Type::BT_BOT) return("bot");
  //else if(tp.bt() == Type::BT_TOP) return("top");
  else if(tp.isint()) return ("int");
  else if(tp.isfloat()) return ("float");
  else if(tp.isbool()) return ("bool");
  else if(tp.isstring()) return ("string");
  else if(tp.is_set()){
    if(tp.bt() == Type::BT_INT) return ("set of int");
    else if(tp.bt() == Type::BT_FLOAT) return ("set of float");
    else if(tp.bt() == Type::BT_BOOL) return ("set of bool");
    else if(tp.bt() == Type::BT_STRING) return ("set of string");
    else return ("unknown set");
  }else if(tp.dim() >= 1  && !tp.is_set()){
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
    int n_genrtrs =  cp->n_generators();
    for(int i = 0; i<n_genrtrs;i++){
        List Gentr;
        List declLists;
        List inList;
        List whereList;
        CharacterVector gtrnms;
        CharacterVector dnms;
        Expression *inExp = cp->in(i);
        Expression *whereExp = cp->where(i);
        for(int j = 0; j < cp->n_decls(i); j++){
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
    for(int k = 0; k < cl->n_args(); k++){
      List cArg;
      exp_details(cl->arg(k), cArg);
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
    for(int i=0; i < uo->n_args(); i++) {
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
      exp_details(ite->e_if(i), ifExp);
      ifExps.push_back(ifExp);
      string iF = "IF";
      iF.append(to_string(i+1));
      ifnms.push_back(iF);
      
      List thenExp;
      exp_details(ite->e_then(i), thenExp);
      thenExps.push_back(thenExp);
      string then = "THEN";
      then.append(to_string(i+1));
      thennms.push_back(then);
    }
    ifExps.names() = ifnms;
    thenExps.names() = thennms;
    List elseExp;
    exp_details(ite->e_else(), elseExp);
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
    if(tExp->type().ispar()){
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