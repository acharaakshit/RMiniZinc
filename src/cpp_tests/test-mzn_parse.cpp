#include <testthat.h>
#include <Rcpp.h>
#include <libgen.h>
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
   expect_error(mzn_parse("", "../../inst/mzn_examples/knapsack/knapsack_0.dzn")); 
  }
}

context("test if correct missing parameter values are returned"){
  test_that("tests for 1d integer array knapsack problem"){
    char* path = getenv("PWD");
    string Path = (string) path;
    string mznpath;
    static const size_t npos = -1;
    size_t slash = Path.find_last_of("/");
    string dirPath = (slash != npos) ? Path.substr(0, slash) : Path;  
    if(dirPath.find("rminizinc.Rcheck") != npos){
      
      slash = dirPath.find_last_of("/");
      dirPath = (slash != npos) ? dirPath.substr(0, slash) : dirPath;
      string dest = dirPath;
      if(dirPath.find("RMiniZinc") != npos){
        // for travis
        mznpath = dest.append("/inst/mzn_examples/knapsack/knapsack_0.mzn");
      }else{
        // for R CMD CHECK
        mznpath = dest.append("/RMiniZinc/inst/mzn_examples/knapsack/knapsack_0.mzn"); 
      }
    }else{
      // for devtools::test()
      mznpath = "../../inst/mzn_examples/knapsack/knapsack_0.mzn";
    }
    
    List parseVal = mzn_parse("",mznpath);
    
    // all the fields are present
    expect_true(parseVal.length() == 4);
    
    //parameter checks
    List pars = parseVal["VARIABLES"];
    vector<string> vNames;
    for(int i = 0;i<pars.length();i++){
      string decl = "DECL";
      decl.append(to_string(i+1));
      List vDetails = pars[decl];
      List vDetNames = vDetails["DETAILS"];
      vNames.push_back(vDetNames["NAME"]);
    }
    
    vector<string> cmpWith = {"n", "OBJ", "capacity", "profit", "size", "x"};
    expect_true(std::equal(vNames.begin(), vNames.end(), cmpWith.begin()));
    
  }
  test_that("tests for bool knapsack problem"){
    char* path = getenv("PWD");
    string Path = (string) path;
    string mznpath;
    static const size_t npos = -1;
    size_t slash = Path.find_last_of("/");
    string dirPath = (slash != npos) ? Path.substr(0, slash) : Path;  
    if(dirPath.find("rminizinc.Rcheck") != npos){
      
      slash = dirPath.find_last_of("/");
      dirPath = (slash != npos) ? dirPath.substr(0, slash) : dirPath;
      string dest = dirPath;
      if(dirPath.find("RMiniZinc") != npos){
        // for travis
        mznpath = dest.append("/inst/mzn_examples/knapsack/knapsack_2(bool).mzn");
      }else{
        // for R CMD CHECK
        mznpath = dest.append("/RMiniZinc/inst/mzn_examples/knapsack/knapsack_2(bool).mzn"); 
      }
    }else{
      // for devtools::test()
      mznpath = "../../inst/mzn_examples/knapsack/knapsack_2(bool).mzn";
    }
    
    List parseVal = mzn_parse("",mznpath);
    
    // all the fields are present
    expect_true(parseVal.length() == 4);
    
    //parameter checks
    List pars = parseVal["VARIABLES"];
    vector<string> vNames;
    for(int i = 0;i<pars.length();i++){
      string decl = "DECL";
      decl.append(to_string(i+1));
      List vDetails = pars[decl];
      List vDetNames = vDetails["DETAILS"];
      vNames.push_back(vDetNames["NAME"]);
    }
    
    vector<string> cmpWith = {"n", "capacity", "profit", "size", "x"};
    expect_true(std::equal(vNames.begin(), vNames.end(), cmpWith.begin()));
    
  }    
      
  test_that("tests for set knapsack problem"){
    char* path = getenv("PWD");
    string Path = (string) path;
    string mznpath;
    static const size_t npos = -1;
    size_t slash = Path.find_last_of("/");
    string dirPath = (slash != npos) ? Path.substr(0, slash) : Path;  
    if(dirPath.find("rminizinc.Rcheck") != npos){
      
      slash = dirPath.find_last_of("/");
      dirPath = (slash != npos) ? dirPath.substr(0, slash) : dirPath;
      string dest = dirPath;
      if(dirPath.find("RMiniZinc") != npos){
        // for travis
        mznpath = dest.append("/inst/mzn_examples/knapsack/knapsack_3(set_concise).mzn");
      }else{
        // for R CMD CHECK
        mznpath = dest.append("/RMiniZinc/inst/mzn_examples/knapsack/knapsack_3(set_concise).mzn"); 
      }
    }else{
      // for devtools::test()
      mznpath = "../../inst/mzn_examples/knapsack/knapsack_3(set_concise).mzn";
    }
    
    List parseVal = mzn_parse("",mznpath);
    
    // all the fields are present
    expect_true(parseVal.length() == 4);
    
    //parameter checks
    List pars = parseVal["VARIABLES"];
    vector<string> vNames;
    for(int i = 0;i<pars.length();i++){
      string decl = "DECL";
      decl.append(to_string(i+1));
      List vDetails = pars[decl];
      List vDetNames = vDetails["DETAILS"];
      vNames.push_back(vDetNames["NAME"]);
    }
    
    vector<string> cmpWith = {"n", "OBJ", "capacity", "profit", "size", "x"};
    expect_true(std::equal(vNames.begin(), vNames.end(), cmpWith.begin()));
  }
}  

