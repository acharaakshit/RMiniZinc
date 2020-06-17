#include <testthat.h>
#include <Rcpp.h>
#include <../src/set_params.h>

using namespace std;


context("Correct parameters should be provided") {
  
  test_that("all missing values should be provided"){
    modelString = "int: a; int: b;";
    List modData   = NumericVector({10});
    expect_error(set_params(modData, modelString));
  }
  test_that("correct type of value should be provided"){
    modelString = "int: a; int: b;";
    List modData({10, "m"});
    modData.names() = CharacterVector({"a", "b"})
    expect_error(set_params(modData, modelString));
  }
}