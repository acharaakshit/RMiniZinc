#include <testthat.h>
#include <Rcpp.h>
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
    string mznpath = "../../mzn_test_examples/knapsack/knapsack_0.mzn";
    List parseVal = mzn_parse("",mznpath);
    expect_true(parseVal.length() == 2);
    CharacterVector missingVals = parseVal["missingValues"];
    expect_true(missingVals.length() == 4);
  }
}

