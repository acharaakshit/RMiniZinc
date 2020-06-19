#include <Rcpp.h>
#include "filetoString.h"

using namespace Rcpp;
using namespace std;

//' @title convert filename to string
//' 
//' @description this function reads a file from a given file path
//' and reads it.
//' 
//' @importFrom Rcpp sourceCpp
//' @export filetoString
//' @useDynLib rminizinc, .registration=TRUE
//' @param filepath the path of mzn file.
// [[Rcpp::export]]
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
      Rcpp::stop(e.what());
    }
    if(modelString.empty()) Rcpp::stop("Empty file given");
    return modelString;
}



