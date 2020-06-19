#include <testthat.h>
#include <Rcpp.h>
#include "../src/mzn_parse.h"

context("Correct mzn path input tests") {

  test_that("empty function call yeilds an error") {
    expect_error(mzn_parse());
  }
  
  test_that("incorrect mzn filename yeilds an error"){
    expect_error(mzn_parse("","notAfilename",""));
  }
  test_that("syntax errors yeild an error"){
    expect_error(mzn_parse("int a = 10"));
  }
}
