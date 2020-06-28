library(rminizinc)

mzn_path = "mzn_examples/assign/assign_inverse.mzn"

missingPars = getMissingPars(mznpath = mzn_path)

pVals = list(4, 4, matrix(c(7, 1, 3, 4, 8, 2, 5, 1, 4, 3, 7, 2, 3, 1, 6, 3), nrow = 4, ncol = 4, byrow = TRUE))
names(pVals) = missingPars

modString = set_params(modData = pVals, mznpath = mzn_path, modify_mzn = FALSE)

solution  = mzn_eval(modelString = modString, solver = "org.gecode.gecode",
            libpath = "/snap/minizinc/current/share/minizinc")

print(solution$Solutions$optimal_solution)