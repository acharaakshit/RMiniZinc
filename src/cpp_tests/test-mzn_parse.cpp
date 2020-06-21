#include <testthat.h>
#include <Rcpp.h>
#include <libgen.h>
#include "../src/mzn_parse.h"

using namespace std;
using namespace Rcpp;

context("Correct mzn path input tests") {

  test_that("empty function call yeilds an error") {
    expect_error(mzn_parse());
  }
  
  test_that("incorrect mzn filename yeilds an error"){
    expect_error(mzn_parse("","notAfilename",""));
  }
  test_that("syntax errors yeild an error"){
    expect_error(mzn_parse("", "int a = 10"));
  }
  test_that("incorrect file extention yeilds error"){
   expect_error(mzn_parse("", "../../mzn_test_examples/knapsack/knapsack_0.dzn")); 
  }
}

context("test if correct missing parameter values are returned"){
  test_that("tests for 1d integer array knapsack problem"){
    char* path = getenv("PWD");
    string Path = (string) path;
    string mznpath;
    static const size_t npos = -1;
    size_t slash = Path.find_last_of("/");
    string dirPath = (slash != npos) ? Path.substr(0, slash) : Path;  
    
    if(dirPath.find("rminizinc.Rcheck") != npos){
      slash = dirPath.find_last_of("/");
      dirPath = (slash != npos) ? dirPath.substr(0, slash) : dirPath;
      string destmzn = dirPath;
      mznpath = destmzn.append("/RMiniZinc/mzn_test_examples/knapsack/knapsack_0.mzn");
    }else{
      mznpath = "../../mzn_test_examples/knapsack/knapsack_0.mzn";
    }
    
    List parseVal = mzn_parse("",mznpath);
    expect_true(parseVal.length() == 2);
    CharacterVector missingVals = parseVal["missingValues"];
    expect_true(missingVals.length() == 4);
  }
}

