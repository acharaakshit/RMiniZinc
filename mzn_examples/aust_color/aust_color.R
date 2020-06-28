library(rminizinc)

mzn_path = "mzn_examples/aust_color/aust_colord.mzn"

missingPars = getMissingPars(mznpath = mzn_path)

pVals = list(4)
names(pVals) = "nc"

modString = set_params(modData = pVals, mznpath = mzn_path, modify_mzn = FALSE)


solution  = mzn_eval(modelString = modString,
              solver = "org.gecode.gecode",
              libpath = "/snap/minizinc/current/share/minizinc")

print(solution$Solutions$optimal_solution)
