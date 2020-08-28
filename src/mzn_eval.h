#ifndef PKG_RMINIZINC_H
#define PKG_RMINIZINC_H

Rcpp::List mzn_eval(std::string solver, std::string libpath, 
                    Rcpp::Environment &Rmodel, std::string dznpath = "",
                    bool all_solutions = true, int time_limit = 300000);

#endif
