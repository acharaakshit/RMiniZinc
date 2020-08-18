#include <testthat.h>
#include <Rcpp.h>
#include <libgen.h>
#include "../src/set_params.h"
#include "../src/helper_parse.h"

using namespace std;
using namespace Rcpp;

context("Correct parameters should be provided") {
  
  test_that("values more than missing values can't be provided"){
    string modelString = "int: a; int: b;";
    List modData = (List)NumericVector({10, 20, 30});
    modData.names() = CharacterVector({"a", "b", "c"});
    expect_error(set_params(modData, modelString));
  }
  test_that("correct names of parameters should be provided"){
    string modelString = "int: a; int: b;";
    List modData = (List)NumericVector({10, 20});
    modData.names() = CharacterVector({"a", "c"});
    expect_error(set_params(modData, modelString));
  }
  test_that("both modelString and filename can't be provided"){
    List modData;
    modData.push_back(10);
    expect_error(set_params(modData, "int: a =10;","abc.mzn"));
    
  }
}

context("tests for optimization problems"){
  test_that("parameters are changed for knapsack problem"){
    char* path = getenv("PWD");
    string Path = (string) path;
    string mznpath;
    static const size_t npos = -1;
    size_t slash = Path.find_last_of("/");
    string dirPath = (slash != npos) ? Path.substr(0, slash) : Path;  
    //string travisPath = "notFound";
    //if(getenv("RCHECK_DIR") != NULL)
    //  travisPath = (string) getenv("RCHECK_DIR");
    if(dirPath.find("rminizinc.Rcheck") != npos){
      
      slash = dirPath.find_last_of("/");
      dirPath = (slash != npos) ? dirPath.substr(0, slash) : dirPath;
      string destmzn = dirPath;
      if(dirPath.find("RMiniZinc") != npos){
        // for travis
        mznpath = destmzn.append("/inst/extdata/mzn_examples/knapsack/knapsack_0.mzn");
      }else{
        // for R CMD CHECK
        mznpath = destmzn.append("/RMiniZinc/inst/extdata/mzn_examples/knapsack/knapsack_0.mzn");  
      }
    }else{
      // for devtools::test()
      mznpath = "../../inst/extdata/mzn_examples/knapsack/knapsack_0.mzn";
    }
    string modelString = filetoString(mznpath);
    List modData = (List)NumericVector({3,9});
    modData.push_back(NumericVector({15,10,7}));
    modData.push_back(NumericVector({4, 3, 2}));
    modData.names() = CharacterVector({"n", "capacity", "profit", "size"});
    // set the parameters and get the updated string
    string updatedModelString = set_params(modData, modelString);
    // once all the parameters are set, this should result in an error
    expect_error(set_params(modData, updatedModelString));
  }
  
  test_that("parameters are changed for production planning problem"){
    char* path = getenv("PWD");
    string Path = (string) path;
    string mznpath;
    static const size_t npos = -1;
    size_t slash = Path.find_last_of("/");
    string dirPath = (slash != npos) ? Path.substr(0, slash) : Path;  
    //string travisPath = "notFound";
    //if(getenv("RCHECK_DIR") != NULL)
    //  travisPath = (string) getenv("RCHECK_DIR");
    if(dirPath.find("rminizinc.Rcheck") != npos){
      
      slash = dirPath.find_last_of("/");
      dirPath = (slash != npos) ? dirPath.substr(0, slash) : dirPath;
      string destmzn = dirPath;
      if(dirPath.find("RMiniZinc") != npos){
        // for travis
        mznpath = destmzn.append("/inst/extdata/mzn_examples/production_planning/prod_plan_0.mzn");
      }else{
        // for R CMD CHECK
        mznpath = destmzn.append("/RMiniZinc/inst/extdata/mzn_examples/production_planning/prod_plan_0.mzn");  
      }
    }else{
      // for devtools::test()
      mznpath = "../../inst/extdata/mzn_examples/production_planning/prod_plan_0.mzn";
    }
    string modelString = filetoString(mznpath);
    
    List modData = (List)NumericVector({2, 5});
    modData.push_back(NumericVector({400,500}));
    modData.push_back(NumericVector({4000, 6, 2000, 500, 500}));
    modData.push_back(CharacterVector({"banana-cake", "chocolate-cake"}));
    modData.push_back(CharacterVector({"flour","banana","sugar","butter","cocoa"}));
    NumericVector consumption = {250, 2, 75, 100, 0, 200, 0, 150, 150, 75};
    modData.push_back(consumption);
    modData.names() = CharacterVector({"nproducts", "nresources", "profit", "capacity", "pname", "rname", "consumption"});
    // set the parameters and get the updated string
    string updatedModelString = set_params(modData, modelString);
    // once all the parameters are set, this should result in an error
    expect_error(set_params(modData, updatedModelString));
  }
}

