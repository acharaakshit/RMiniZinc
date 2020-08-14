#include "minizinc/parser.hh"
#include <Rcpp.h>

int dirExists(const char* const path);
MiniZinc::Model* helper_parse(std::string modelString, std::string modelStringName,
                              std::vector<std::string> includePath);