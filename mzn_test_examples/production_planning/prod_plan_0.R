library(rminizinc)
library(checkmate)

# give a file path to read the mzn and write the updated mzn
mznName = "mzn_test_examples/production_planning/prod_plan_0_update.mzn"

# parse the model
missingVals <- rminizinc:::getMissingPars(mznpath = mznName)

print(missingVals)

# give values to some of the parameters
pVals1 = list(2,c(400, 500))
names(pVals1) = c(missingVals[1:2])

# set the parameters
rminizinc:::set_params(modData = pVals1, mznpath = mznName, modify_mzn = TRUE)

# find the missing parameters again
missingVals = rminizinc:::getMissingPars(mznpath = mznName)

assert(test_set_equal(missingVals, c("pname", "nresources", "capacity", "rname", "consumption")))

# give values to all the missing parameters
pVals2 = list(c("banana-cake", "chocolate-cake"),5, c(4000, 6, 2000, 500, 500))
names(pVals2) = missingVals[1:3]

rminizinc:::set_params(modData = pVals2, mznpath = mznName, modify_mzn = TRUE)

# check if there are any missing parameters
missingVals = rminizinc::getMissingPars(mznpath = mznName)

assert(test_set_equal(missingVals, c("rname", "consumption")))

# use matrix for providing values to 2d arrays
pVals3 = list(c("flour","banana","sugar","butter","cocoa"), 
              matrix(c(250, 2, 75, 100, 0, 200, 0, 150, 150, 75), nrow = 2, ncol = 5, byrow = TRUE))
names(pVals3) = missingVals

modString = rminizinc:::set_params(modData = pVals3, mznpath = mznName, modify_mzn = TRUE)

missingVals = rminizinc:::getMissingPars(mznpath = mznName)

assert_true(length(missingVals) == 0)

# get the solutions
solution = rminizinc:::mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                                libpath = "/snap/minizinc/current/share/minizinc")

# get the optimal solution
print(solution$Solutions$optimal_solution)

