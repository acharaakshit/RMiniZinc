#include <minizinc/ast.hh>
#include "helper_sol_parse.h"

void expDetails(MiniZinc::Expression *exp, Rcpp::List &cstNames);
std::string boStrMap(MiniZinc::BinOpType OP);
std::string uoStrMap(MiniZinc::UnOpType OP);
std::string vType(MiniZinc::Type tp);