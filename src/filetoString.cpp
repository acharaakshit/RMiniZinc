#include <Rcpp.h>
#include "filetoString.h"

using namespace Rcpp;
using namespace std;

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



