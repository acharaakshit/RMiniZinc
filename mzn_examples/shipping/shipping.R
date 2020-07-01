library(rminizinc)
library(checkmate)

# path of mzn
mzn_path = "mzn_examples/shipping/shipping_update.mzn"

missingVals = rminizinc::getMissingPars(mznpath = mzn_path )

pVals = list(4,3,c(30, 20, 35, 20), c(40, 40, 25), 
             c(6, 5, 7, 1, 3, 4, 2, 1, 7, 3, 9, 5))
names(pVals) = missingVals

# change to true to see updates in the mzn
modString = rminizinc:::set_params(pVals,mznpath = mzn_path, modify_mzn = FALSE)

# get the solutions
solution = rminizinc:::mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                                  libpath = "/snap/minizinc/current/share/minizinc",
                                  all_solutions = FALSE)
