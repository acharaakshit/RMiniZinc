#include "minizinc/parser.hh"
#include <Rcpp.h>

std::string filetoString(std::string filepath);
std::string pathStringcheck(std::string modelString, std::string mznpath);
int dirExists(const char* const path);
MiniZinc::Model* helper_parse(std::string modelString, std::string modelStringName,
                              std::vector<std::string> includePath);