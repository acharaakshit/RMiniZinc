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
   expect_error(mzn_parse("", "../../mzn_examples/knapsack/knapsack_0.dzn")); 
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
        mznpath = dest.append("/mzn_examples/knapsack/knapsack_0.mzn");
      }else{
        // for R CMD CHECK
        mznpath = dest.append("/RMiniZinc/mzn_examples/knapsack/knapsack_0.mzn"); 
      }
    }else{
      // for devtools::test()
      mznpath = "../../mzn_examples/knapsack/knapsack_0.mzn";
    }
    
    List parseVal = mzn_parse("",mznpath);
    
    // all the fields are present
    expect_true(parseVal.length() == 4);
    
    //parameter checks
    List pars = parseVal["Variables"];
    vector<string> vNames;
    for(int i = 0;i<pars.length();i++){
      string decl = "decl";
      decl.append(to_string(i+1));
      List vDetails = pars[decl];
      vNames.push_back(vDetails["name"]);
    }
    
    vector<string> cmpWith = {"n", "OBJ", "capacity", "profit", "size", "x"};
    expect_true(std::equal(vNames.begin(), vNames.end(), cmpWith.begin()));
    
    // constraint checks
    List constraints = parseVal["Constraints"];
    List cDetails = constraints["constraint1"];
    vector<string>  cstvNames = cDetails["varsInvolved"];
    
    vector<string> compareWith = {"OBJ","i", "x"};
    expect_true(cstvNames.size() == compareWith.size());
    expect_true(std::equal(cstvNames.begin(), cstvNames.end(), compareWith.begin()));
    
    List cDetails2 = constraints["constraint2"];
    vector<string>  cstvNames2 = cDetails2["varsInvolved"];
    vector<string> compareWith2 = {"OBJ", "capacity", "i", "size", "x"};
    expect_true(std::equal(cstvNames2.begin(), cstvNames2.end(), compareWith2.begin()));
    
    //solve type checks
    List st = parseVal["SolveType"];
    string objective = st[0];
    if(objective == "satisfy")
      expect_true(st.length() == 2);
    else{
      expect_true(st.length() == 3);
      CharacterVector slvNames = st[2];
      vector<string> compareWith = {"OBJ", "i", "profit", "x" };
      expect_true(std::equal(slvNames.begin(), slvNames.end(), compareWith.begin()));
    }
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
        mznpath = dest.append("/mzn_examples/knapsack/knapsack_2(bool).mzn");
      }else{
        // for R CMD CHECK
        mznpath = dest.append("/RMiniZinc/mzn_examples/knapsack/knapsack_2(bool).mzn"); 
      }
    }else{
      // for devtools::test()
      mznpath = "../../mzn_examples/knapsack/knapsack_2(bool).mzn";
    }
    
    List parseVal = mzn_parse("",mznpath);
    
    // all the fields are present
    expect_true(parseVal.length() == 4);
    
    //parameter checks
    List pars = parseVal["Variables"];
    vector<string> vNames;
    for(int i = 0;i<pars.length();i++){
      string decl = "decl";
      decl.append(to_string(i+1));
      List vDetails = pars[decl];
      vNames.push_back(vDetails["name"]);
    }
    
    vector<string> cmpWith = {"n", "capacity", "profit", "size", "x"};
    expect_true(std::equal(vNames.begin(), vNames.end(), cmpWith.begin()));
    
    // constraint checks
    List constraints = parseVal["Constraints"];
    List cDetails = constraints["constraint1"];
    vector<string>  cstvNames = cDetails["varsInvolved"];
  
    vector<string> compareWith = {"capacity", "i", "n", "size", "x"};
    expect_true(cstvNames.size() == compareWith.size());
    expect_true(std::equal(cstvNames.begin(), cstvNames.end(), compareWith.begin()));
    
    //solve type checks
    List st = parseVal["SolveType"];
    string objective = st[0];
    if(objective == "satisfy")
      expect_true(st.length() == 2); 
    else{
      expect_true(st.length() == 3);
      CharacterVector slvNames = st[2];
      vector<string> compareWith = {"i", "n", "profit", "x" };
      expect_true(std::equal(slvNames.begin(), slvNames.end(), compareWith.begin()));
    }
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
        mznpath = dest.append("/mzn_examples/knapsack/knapsack_3(set_concise).mzn");
      }else{
        // for R CMD CHECK
        mznpath = dest.append("/RMiniZinc/mzn_examples/knapsack/knapsack_3(set_concise).mzn"); 
      }
    }else{
      // for devtools::test()
      mznpath = "../../mzn_examples/knapsack/knapsack_3(set_concise).mzn";
    }
    
    List parseVal = mzn_parse("",mznpath);
    
    // all the fields are present
    expect_true(parseVal.length() == 4);
    
    //parameter checks
    List pars = parseVal["Variables"];
    vector<string> vNames;
    for(int i = 0;i<pars.length();i++){
      string decl = "decl";
      decl.append(to_string(i+1));
      List vDetails = pars[decl];
      vNames.push_back(vDetails["name"]);
    }
    
    vector<string> cmpWith = {"n", "OBJ", "capacity", "profit", "size", "x"};
    expect_true(std::equal(vNames.begin(), vNames.end(), cmpWith.begin()));
    
    // constraint checks
    List constraints = parseVal["Constraints"];
    List cDetails = constraints["constraint1"];
    vector<string>  cstvNames = cDetails["varsInvolved"];
    
    vector<string> compareWith = {"capacity", "i", "size", "x"};
    expect_true(cstvNames.size() == compareWith.size());
    expect_true(std::equal(cstvNames.begin(), cstvNames.end(), compareWith.begin()));
    
    //solve type checks
    List st = parseVal["SolveType"];
    string objective = st[0];
    if(objective == "satisfy")
      expect_true(st.length() == 1); 
    else{
      expect_true(st.length() == 3);
      CharacterVector slvNames = st[2];
      vector<string> compareWith = {"i", "profit", "x"};
      expect_true(std::equal(slvNames.begin(), slvNames.end(), compareWith.begin()));
    }
  }
}  

