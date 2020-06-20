#include <testthat.h>
#include <Rcpp.h>
#include <../src/set_params.h>
#include "../src/filetoString.h"

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
  test_that("parameters are changed"){
    string mznpath = "../../mzn_test_examples/knapsack/knapsack_0.mzn";
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
}
