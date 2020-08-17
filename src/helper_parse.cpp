#include "helper_parse.h"
#include <fstream>
#include <sys/types.h>
#include <sys/stat.h>

using namespace std;
using namespace MiniZinc;
using namespace Rcpp;

// helper function to check if provided directory path is correct
int dirExists(const char* const path){
  struct stat info;
  
  int statRC = stat( path, &info );
  if( statRC != 0 )
  {
    if (errno == ENOENT)  { return 0; } // something along the path does not exist
    if (errno == ENOTDIR) { return 0; } // something in path prefix is not a dir
    return -1;
  }
  
  return ( info.st_mode & S_IFDIR ) ? 1 : 0;
}

MiniZinc::Model* helper_parse(std::string modelString, std::string modelStringName,
                              std::vector<std::string> includePath){
  
  for(int i = 0; i < includePath.size(); i++){
    int retIP = dirExists(includePath[i].c_str());
    if(retIP == 0){
      Rcpp::stop("Include path directory doesn't exist or not a directory");
    }else if(retIP < 0){
      Rcpp::stop("Error occured in include path");
    }
  }
  
  Env* env = new Env();
  // include paths of mzn files to be included
  vector<SyntaxError> se;
  Model *model;
  
  Rcpp::Environment utils("package:utils");
  Rcpp::Function utils_cpp = utils["data"];  
  utils_cpp("config");
  std::string mk =  Rcpp::as<string>(Environment::global_env()["LIBMINIZINC_PATH"]);
  mk.reserve(50);    

  size_t start = mk.find("-L");
  size_t end = mk.find("libminizinc", start);
  string sub = mk.substr(start + 2, end - start - 2);
  sub.append("libminizinc/share/minizinc/std");
  includePath.push_back(sub);
  if(includePath.size() == 1){
    Rcout << "Using default include path: " << sub << endl;
  }
  try{
    std::stringstream ss;
    //change the underlying buffer and save the old buffer
    auto old_buf = std::cerr.rdbuf(ss.rdbuf()); 
    model = MiniZinc::parseFromString(*env, modelString, modelStringName , includePath,
                                      true, true, true, Rcpp::Rcerr, se);
    std::cerr.rdbuf(old_buf); //reset
    Rcerr << ss.str();
    if(model==NULL) throw std::exception();
    else if(model->size() == 0) Rcpp::stop("Empty Model!");
  }catch(std::exception& e){
    Rcpp::stop("NULL model !");
  }
  return model;
}
