#include <testthat.h>
#include <Rcpp.h>
#include "../src/getMissingPars.h"

using namespace Rcpp;
using namespace std;


context("verify the missing values"){
  test_that("verify the missing vals for first knapsack problem"){
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
        mznpath = dest.append("/mzn_examples/knapsack/knapsack_0.mzn");
      }else{
        // for R CMD CHECK
        mznpath = dest.append("/RMiniZinc/mzn_examples/knapsack/knapsack_0.mzn"); 
      }
    }else{
      // for devtools::test()
      mznpath = "../../mzn_examples/knapsack/knapsack_0.mzn";
    }
    // missing parameter checks
    CharacterVector missingVals = getMissingPars("", mznpath);
    expect_true(missingVals.size() == 4);
    
    vector<string> compareWith = {"n", "capacity", "profit", "size"};
    expect_true(std::equal(missingVals.begin(), missingVals.end(), compareWith.begin()));
  }
  
  test_that("verify the missing vals for boolean knapsack problem"){
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
        mznpath = dest.append("/mzn_examples/knapsack/knapsack_2(bool).mzn");
      }else{
        // for R CMD CHECK
        mznpath = dest.append("/RMiniZinc/mzn_examples/knapsack/knapsack_2(bool).mzn"); 
      }
    }else{
      // for devtools::test()
      mznpath = "../../mzn_examples/knapsack/knapsack_2(bool).mzn";
    }
    // missing parameter checks
    CharacterVector missingVals = getMissingPars("", mznpath);
    expect_true(missingVals.size() == 4);
    
    vector<string> compareWith = {"n", "capacity", "profit", "size"};
    expect_true(std::equal(missingVals.begin(), missingVals.end(), compareWith.begin()));
  }
  
  test_that("verify the missing vals for set knapsack problem"){
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
        mznpath = dest.append("/mzn_examples/knapsack/knapsack_3(set_concise).mzn");
      }else{
        // for R CMD CHECK
        mznpath = dest.append("/RMiniZinc/mzn_examples/knapsack/knapsack_3(set_concise).mzn"); 
      }
    }else{
      // for devtools::test()
      mznpath = "../../mzn_examples/knapsack/knapsack_3(set_concise).mzn";
    }
    // missing parameter checks
    CharacterVector missingVals = getMissingPars("", mznpath);
    expect_true(missingVals.size() == 4);
    
    vector<string> compareWith = {"n", "capacity", "profit", "size"};
    expect_true(std::equal(missingVals.begin(), missingVals.end(), compareWith.begin()));
    
    
  }
}