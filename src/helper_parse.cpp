#include <fstream>
#include <sys/types.h>
#include <sys/stat.h>
#include <Rcpp.h>
#include "helper_parse.h"


using namespace std;
using namespace Rcpp;


// convert contents of file to string
std::string filetoString(std::string filepath) {
  string modelString;
  try{
    ifstream getMzn;
    getMzn.open(filepath);
    if (!getMzn)
      throw std::runtime_error("Could not open file");
    string content( (std::istreambuf_iterator<char>(getMzn) ),
                    (std::istreambuf_iterator<char>() ) );
    modelString = content;
  }catch(std::exception &e){
    string errorStr = e.what();
    errorStr.append(": ");
    errorStr.append(filepath);
    Rcpp::stop(errorStr);
  }
  if(modelString.empty()) Rcpp::stop("Empty file given");
  return modelString;
}

// check if mznpath or model string is passed and take appropriate action
std::string pathStringcheck(std::string modelString, std::string mznpath){
  if(modelString.empty() && mznpath.empty()){
    Rcpp::stop("PROVIDE EITHER modelString OR mznpath");
  }else if(!modelString.empty() && !mznpath.empty()){
    Rcpp::stop("PROVIDE ONLY ONE OF modelString OR mznpath");
  }else if(mznpath.length()){
    // check file extension
    if(!(mznpath.substr(mznpath.find_last_of(".") + 1) == "mzn" ))
      Rcpp::stop("file extention is not mzn");
    //convert to string 
    modelString = filetoString(mznpath);
  }
  return (modelString);
}

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

