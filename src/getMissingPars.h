#ifndef PKG_RMINIZINC_H
#define PKG_RMINIZINC_H

Rcpp::CharacterVector getMissingPars(std::string modelString="",
                                     std::string mznpath="",
                                     std::string modelStringName="missing_pars.mzn");

#endif
