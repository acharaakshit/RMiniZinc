#include <testthat.h>
#include <Rcpp.h>
#include <libgen.h>
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
    char* path = getenv("PWD");
    string Path = (string) path;
    string mznpath;
    string dznpath;
    static const size_t npos = -1;
    size_t slash = Path.find_last_of("/");
    string dirPath = (slash != npos) ? Path.substr(0, slash) : Path;  
    if(dirPath.find("rminizinc.Rcheck") != npos){
      
      slash = dirPath.find_last_of("/");
      dirPath = (slash != npos) ? dirPath.substr(0, slash) : dirPath;
      string destmzn = dirPath;
      string destdzn =  dirPath;
      if(dirPath.find("RMiniZinc") != npos){
        // for travis
        mznpath = destmzn.append("/mzn_examples/knapsack/knapsack_0.mzn");
        dznpath = destdzn.append("/mzn_examples/knapsack/knapsack_0.dzn");
      }else{
        // for R CMD CHECK
        mznpath = destmzn.append("/RMiniZinc/mzn_examples/knapsack/knapsack_0.mzn"); 
        dznpath = destdzn.append("/RMiniZinc/mzn_examples/knapsack/knapsack_0.dzn"); 
      }
    }else{
      // for devtools::test()
      mznpath = "../../mzn_examples/knapsack/knapsack_0.mzn";
      dznpath = "../../mzn_examples/knapsack/knapsack_0.dzn";
    }
    
    string solver = "org.gecode.gecode";
    string libpath = "/snap/minizinc/current/share/minizinc";
    List pres = mzn_eval(solver, libpath, "", mznpath, dznpath);
    // check if both solution string and solutions are obtained
    expect_true(pres.length() == 2);
    List psols = pres["SOLUTIONS"];
    // check if all the solutions are obtained
    expect_true(psols.size() == 7);
  }
  test_that("1d boolean array knapsack problem"){
    char* path = getenv("PWD");
    string Path = (string) path;
    string mznpath;
    string dznpath;
    static const size_t npos = -1;
    size_t slash = Path.find_last_of("/");
    string dirPath = (slash != npos) ? Path.substr(0, slash) : Path;  
    if(dirPath.find("rminizinc.Rcheck") != npos){
      
      slash = dirPath.find_last_of("/");
      dirPath = (slash != npos) ? dirPath.substr(0, slash) : dirPath;
      string destmzn = dirPath;
      string destdzn = dirPath;
      if(dirPath.find("RMiniZinc") != npos){
        // for travis
        mznpath = destmzn.append("/mzn_examples/knapsack/knapsack_2(bool).mzn");
        dznpath = destdzn.append("/mzn_examples/knapsack/knapsack_0.dzn");
      }else{
        // for R CMD CHECK
        mznpath = destmzn.append("/RMiniZinc/mzn_examples/knapsack/knapsack_2(bool).mzn"); 
        dznpath = destdzn.append("/RMiniZinc/mzn_examples/knapsack/knapsack_0.dzn"); 
      }
    }else{
      // for devtools::test()
      mznpath = "../../mzn_examples/knapsack/knapsack_2(bool).mzn";
      dznpath = "../../mzn_examples/knapsack/knapsack_0.dzn";
    }
    
    string solver = "org.gecode.gecode";
    string libpath = "/snap/minizinc/current/share/minizinc";
    List pres = mzn_eval(solver, libpath, "", mznpath, dznpath);
    // check if both solution string and solutions are obtained
    expect_true(pres.length() == 2);
    List psols = pres["SOLUTIONS"];
    // check if all the solutions are obtained
    expect_true(psols.size() == 4);
  }
  test_that("knapsack problem involving set works"){
    char* path = getenv("PWD");
    string Path = (string) path;
    string mznpath;
    string dznpath;
    static const size_t npos = -1;
    size_t slash = Path.find_last_of("/");
    string dirPath = (slash != npos) ? Path.substr(0, slash) : Path;  
    if(dirPath.find("rminizinc.Rcheck") != npos){
      
      slash = dirPath.find_last_of("/");
      dirPath = (slash != npos) ? dirPath.substr(0, slash) : dirPath;
      string destmzn = dirPath;
      string destdzn = dirPath;
      if(dirPath.find("RMiniZinc") != npos){
        // for travis
        mznpath = destmzn.append("/mzn_examples/knapsack/knapsack_3(set_concise).mzn");
        dznpath = destdzn.append("/mzn_examples/knapsack/knapsack_2.dzn");
      }else{
        // for R CMD CHECK
        mznpath = destmzn.append("/RMiniZinc/mzn_examples/knapsack/knapsack_3(set_concise).mzn"); 
        dznpath = destdzn.append("/RMiniZinc/mzn_examples/knapsack/knapsack_2.dzn"); 
      }
    }else{
      // for devtools::test()
      mznpath = "../../mzn_examples/knapsack/knapsack_3(set_concise).mzn";
      dznpath = "../../mzn_examples/knapsack/knapsack_2.dzn";
    }
    string solver = "org.gecode.gecode";
    string libpath = "/snap/minizinc/current/share/minizinc";
    List pres = mzn_eval(solver, libpath, "", mznpath, dznpath);
    // check if both solution string and solutions are obtained
    expect_true(pres.length() == 2);
    List psols = pres["SOLUTIONS"];
    // check if all the solutions are obtained
    expect_true(psols.size() == 1);
  }
  
  test_that("production planning problem can be solved"){
    char* path = getenv("PWD");
    string Path = (string) path;
    string mznpath;
    string dznpath;
    static const size_t npos = -1;
    size_t slash = Path.find_last_of("/");
    string dirPath = (slash != npos) ? Path.substr(0, slash) : Path;  
    if(dirPath.find("rminizinc.Rcheck") != npos){
      
      slash = dirPath.find_last_of("/");
      dirPath = (slash != npos) ? dirPath.substr(0, slash) : dirPath;
      string destmzn = dirPath;
      string destdzn = dirPath;
      if(dirPath.find("RMiniZinc") != npos){
        // for travis
        mznpath = destmzn.append("/mzn_examples/production_planning/prod_plan_0.mzn");
        dznpath = destdzn.append("/mzn_examples/production_planning/prod_plan_0.dzn");
      }else{
        // for R CMD CHECK
        mznpath = destmzn.append("/RMiniZinc/mzn_examples/production_planning/prod_plan_0.mzn"); 
        dznpath = destdzn.append("/RMiniZinc/mzn_examples/production_planning/prod_plan_0.dzn"); 
      }
    }else{
      // for devtools::test()
      mznpath = "../../mzn_examples/production_planning/prod_plan_0.mzn";
      dznpath = "../../mzn_examples/production_planning/prod_plan_0.dzn";
    }
    string solver = "org.gecode.gecode";
    string libpath = "/snap/minizinc/current/share/minizinc";
    List pres = mzn_eval(solver, libpath, "", mznpath, dznpath);
    // check if both solution string and solutions are obtained
    expect_true(pres.length() == 2);
    List psols = pres["SOLUTIONS"];
    // check if all the solutions are obtained
    expect_true(psols.size() == 7);
  }
  test_that("shipping problem can be solved"){
    char* path = getenv("PWD");
    string Path = (string) path;
    string mznpath;
    string dznpath;
    static const size_t npos = -1;
    size_t slash = Path.find_last_of("/");
    string dirPath = (slash != npos) ? Path.substr(0, slash) : Path;  
    if(dirPath.find("rminizinc.Rcheck") != npos){
      
      slash = dirPath.find_last_of("/");
      dirPath = (slash != npos) ? dirPath.substr(0, slash) : dirPath;
      string destmzn = dirPath;
      string destdzn = dirPath;
      if(dirPath.find("RMiniZinc") != npos){
        // for travis
        mznpath = destmzn.append("/mzn_examples/shipping/shipping.mzn");
        dznpath = destdzn.append("/mzn_examples/shipping/shipping.dzn");
      }else{
        // for R CMD CHECK
        mznpath = destmzn.append("/RMiniZinc/mzn_examples/shipping/shipping.mzn"); 
        dznpath = destdzn.append("/RMiniZinc/mzn_examples/shipping/shipping.dzn"); 
      }
    }else{
      // for devtools::test()
      mznpath = "../../mzn_examples/shipping/shipping.mzn";
      dznpath = "../../mzn_examples/shipping/shipping.dzn";
    }
    string solver = "org.gecode.gecode";
    string libpath = "/snap/minizinc/current/share/minizinc";
    List pres = mzn_eval(solver, libpath, "", mznpath, dznpath);
    // check if both solution string and solutions are obtained
    expect_true(pres.length() == 2);
    List psols = pres["SOLUTIONS"];
    // check if all the solutions are obtained
    expect_true(psols.size() == 24);
  }
}
