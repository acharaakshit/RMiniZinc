#ifndef PKG_RMINIZINC_H
#define PKG_RMINIZINC_H

Rcpp::List mzn_parse(std::string modelString = "", 
                     std::string mznfilename = "",
                     std::string  modelStringName = "abc.mzn");

#endif
