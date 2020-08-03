#include <testthat.h>
#include <Rcpp.h>
#include <libgen.h>
#include "../src/deleteItem.h"

using namespace std;
using namespace Rcpp;


context("Correct mzn path input tests") {
  
  test_that("item number should be between 0 to number of items") {
    char* path = getenv("PWD");
    string Path = (string) path;
    string mznpath;
    static const size_t npos = -1;
    size_t slash = Path.find_last_of("/");
    string dirPath = (slash != npos) ? Path.substr(0, slash) : Path;  
    if(dirPath.find("rminizinc.Rcheck") != npos){
      
      slash = dirPath.find_last_of("/");
      dirPath = (slash != npos) ? dirPath.substr(0, slash) : dirPath;
      string dest = dirPath;
      if(dirPath.find("RMiniZinc") != npos){
        // for travis
        mznpath = dest.append("/inst/extdata/mzn_examples/knapsack/knapsack_0.mzn");
      }else{
        // for R CMD CHECK
        mznpath = dest.append("/RMiniZinc/inst/extdata/mzn_examples/knapsack/knapsack_0.mzn"); 
      }
    }else{
      // for devtools::test()
      mznpath = "../../inst/extdata/mzn_examples/knapsack/knapsack_0.mzn";
    }
    
    expect_error(deleteItem(1000, "", mznpath));
  }
}