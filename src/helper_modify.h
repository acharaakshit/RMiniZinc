#include <minizinc/model.hh>
#include <minizinc/prettyprinter.hh>
#include <fstream>

MiniZinc::Item* DomainItemCheck(MiniZinc::Model *model, int ItemNo);
std::string getnWriteStr(std::string mznpath, MiniZinc::Model* model, bool modify_mzn);