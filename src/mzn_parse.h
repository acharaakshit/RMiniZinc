#ifndef PKG_RMINIZINC_H
#define PKG_RMINIZINC_H

Rcpp::List mzn_parse(std::string modelString = "", 
                     std::string mznpath = "",
                     std::string  modelStringName = "mzn_parse.mzn",
                     Rcpp::Nullable<std::vector<std::string>> includePath = R_NilValue);

#endif
