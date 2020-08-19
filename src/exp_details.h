#include <minizinc/ast.hh>

void exp_details(MiniZinc::Expression *exp, Rcpp::List &cstNames);
std::string bo_str_map(MiniZinc::BinOpType OP);
std::string uo_str_map(MiniZinc::UnOpType OP);
std::string get_type(MiniZinc::Type tp);