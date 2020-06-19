#ifndef PKG_RMINIZINC_H
#define PKG_RMINIZINC_H

Rcpp::List mzn_eval(std::string solver, std::string libpath,std::string modelString = "", 
              std::string mznpath = "", std::string dznpath = "");

#endif
