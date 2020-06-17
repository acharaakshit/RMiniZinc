#include <testthat.h>
#include <Rcpp.h>
#include "../src/mzn_eval.h"

using namespace std;

context("Correct parameters are provided") {
  
  test_that("only Gecode solver is supported"){
    string modelString = "int: a = 10;";
    string libpath = "/snap/minizinc/current/share/minizinc";
    expect_error(mzn_eval(modelString,"chuffed",libpath));
  }
  
  test_that("correct lib paths should be provided for solver"){
    string modelString = "int: a = 10;";
    expect_error(mzn_eval(modelString,"chuffed","incorrectPath"));
  }
  
  test_that("correct syntax should be provided"){
    string modelString = "int: a  10;";
    string libpath = "/snap/minizinc/current/share/minizinc";
    string solver = "org.gecode.gecode";
    expect_error(mzn_eval(modelString,solver,libpath));
  }
}
