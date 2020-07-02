library(rminizinc)

mznName = "mzn_examples/jobshop/jobshop_2.mzn" 

parseInfo <- mzn_parse(mznpath = mznName)

missingPars = getMissingPars(mznpath = mznName)

pVals = list(3, 4, c(3,3,4,4,4,3,2,2,3,3,3,4),
             c(1,2,3,4,1,3,2,4,4,2,1,3))
names(pVals) = missingPars

modString = set_params(modData = pVals, mznpath = mznName, modify_mzn = FALSE)


mzn_eval(solver = "org.gecode.gecode", libpath = "/snap/minizinc/current/share/minizinc",
         modelString = modString)  
