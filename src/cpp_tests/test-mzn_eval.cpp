#include <testthat.h>
#include <Rcpp.h>
#include "../src/mzn_eval.h"
#include "../src/filetoString.h"

using namespace Rcpp;
using namespace std;

context("Correct parameters are provided") {
  
  test_that("only Gecode solver is supported"){
    string modelString = "int: a = 10;";
    string libpath = "/snap/minizinc/current/share/minizinc";
    expect_error(mzn_eval("chuffed",libpath, modelString));
  }
  
  test_that("correct lib paths should be provided for solver"){
    string modelString = "int: a = 10;";
    expect_error(mzn_eval("chuffed","incorrectPath", modelString));
  }
  
  test_that("correct syntax should be provided"){
    string modelString = "int: a  10;";
    string libpath = "/snap/minizinc/current/share/minizinc";
    string solver = "org.gecode.gecode";
    expect_error(mzn_eval(solver,libpath, modelString));
  }
}

context("Tests for example knapsack problems"){
  test_that("1d integer array knapsack problem"){
    string mznpath = "../../mzn_test_examples/knapsack/knapsack_0.mzn";
    string dznpath = "../../mzn_test_examples/knapsack/knapsack_0.dzn";
    string solver = "org.gecode.gecode";
    string libpath = "/snap/minizinc/current/share/minizinc";
    List pres = mzn_eval(solver, libpath, "", mznpath, dznpath);
    // check if both solution string and solutions are obtained
    expect_true(pres.length() == 2);
    List psols = pres["Solutions"];
    // check if all the solutions are obtained
    expect_true(psols.size() == 7);
  }
  test_that("1d boolean array knapsack problem"){
    string mznpath = "../../mzn_test_examples/knapsack/knapsack_2.mzn";
    string dznpath = "../../mzn_test_examples/knapsack/knapsack_0.dzn";
    string solver = "org.gecode.gecode";
    string libpath = "/snap/minizinc/current/share/minizinc";
    List pres = mzn_eval(solver, libpath, "", mznpath, dznpath);
    // check if both solution string and solutions are obtained
    expect_true(pres.length() == 2);
    List psols = pres["Solutions"];
    // check if all the solutions are obtained
    expect_true(psols.size() == 4);
  }
  test_that("knapsack problem involving set works"){
    string mznpath = "../../mzn_test_examples/knapsack/knapsack_3(set_concise).mzn";
    string dznpath = "../../mzn_test_examples/knapsack/knapsack_2.dzn";
    string solver = "org.gecode.gecode";
    string libpath = "/snap/minizinc/current/share/minizinc";
    List pres = mzn_eval(solver, libpath, "", mznpath, dznpath);
    // check if both solution string and solutions are obtained
    expect_true(pres.length() == 2);
    List psols = pres["Solutions"];
    // check if all the solutions are obtained
    expect_true(psols.size() == 1);
  }
}
