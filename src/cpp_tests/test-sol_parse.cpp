#include <testthat.h>
#include <Rcpp.h>
#include "../src/sol_parse.h"


using namespace Rcpp;

context("Correct solution string") {
  
  test_that("solution string should have a seperator"){
    expect_error(sol_parse("a = 10;"));
  }
  
  test_that("solution can contain only assignments"){
    expect_error(sol_parse("int: a = 10; \n----------\n=========="));
  }
  
  test_that("Unsatisfiable leads to an error"){
    expect_error(sol_parse("=====UNSATISFIABLE====="));
  }

  test_that("Error is also detected"){
    expect_error(sol_parse("=====ERROR====="));
  }
  
  test_that("solution string is correctly parsed"){
    List sVal = sol_parse("x = [0, 0, 0]\n----------\nx = [0, 0, 1]\n----------\nx = [0, 0, 2]\n----------\nx = [0, 0, 3]\n----------\nx = [0, 0, 4]\n----------\nx = [0, 1, 3]\n----------\nx = [1, 1, 1]\n----------\n==========\n");
    expect_true(sVal.length() == 7);
  }
}
