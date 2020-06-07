#include <Rcpp.h>
#include <minizinc/solver.hh>

using namespace std;
using namespace MiniZinc;
using namespace Rcpp;

//' @title MiniZinc model evaluation
//' 
//' @description evaluates the MiniZinc model
//'
//' @importFrom Rcpp sourceCpp
//' @export mzn_eval
//' @useDynLib rminizinc, .registration=TRUE
//' @param modelString the string representation of the model to be evaluated
// [[Rcpp::export]]
void mzn_eval(const char* modelString){
  //This is under development
  //vector<std::string> options({"--solver", "org.gecode.gecode"});
  //MznSolver slv(std::cout,std::cerr);
  //slv.run(options,modelString, "minizinc", "mymodel.mzn");
  
}




