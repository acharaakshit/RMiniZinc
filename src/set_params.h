#ifndef PKG_RMINIZINC_H
#define PKG_RMINIZINC_H

std::string set_params(Rcpp::List modData, std::string modelString = "",
                       std::string mznpath = "", bool modify_mzn = false);

#endif
