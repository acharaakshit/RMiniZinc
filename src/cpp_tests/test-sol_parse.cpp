#include <testthat.h>
#include <Rcpp.h>
#include "../src/sol_parse.h"

context("Correct solution string") {
  
  test_that("solution string should have a seperator"){
    expect_error(sol_parse("a = 10"));
  }
  
  test_that("solution can contain only assignments"){
    expect_error(sol_parse("int: a = 10; \n----------\n=========="));
  }
  
  test_that("Unsatisfiable leads to an error"){
    expect_error(sol_parse("=====UNSATISFIABLE====="));
  }
  
}
