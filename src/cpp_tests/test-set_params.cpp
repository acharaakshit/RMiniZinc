#include <testthat.h>
#include <Rcpp.h>
#include <../src/set_params.h>

using namespace std;
using namespace Rcpp;

context("Correct parameters should be provided") {
  
  test_that("all missing values should be provided"){
    string modelString = "int: a; int: b;";
    List modData;
    modData.push_back(10);
    expect_error(set_params(modData, modelString));
  }
  test_that("correct type of value should be provided"){
    string modelString = "int: a; int: b;";
    List modData;
    modData.push_back(10);
    modData.push_back("m");
    //modData.names() = Rcpp::CharacterVector({"a", "b"})
    expect_error(set_params(modData, modelString));
  }
}