library(rminizinc)
library(checkmate)

# give a file path to read the mzn and write the updated mzn
mznName = "mzn_test_examples/knapsack/knapsack_1_update.mzn"
#mznName = "mzn_test_examples/knapsack/knapsack_0_update.mzn"

# parse the model
missingVals <- rminizinc:::getMissingPars(mznpath = mznName)

# give values to some of the parameters
pVals1 = list(3,9)
names(pVals1) = c(missingVals[1:2])

# set the parameters
rminizinc:::set_params(modData = pVals1, mznpath = mznName, modify_mzn = TRUE)

# find the missing parameters again
missingVals = rminizinc:::getMissingPars(mznpath = mznName)

print(missingVals)

# test that the missing values are only capacity and size now
assert(test_set_equal(missingVals, c("profit", "size")))

# give values to all the missing parameters
pVals2 = list(c(15,10,7),c(4,3,2))
names(pVals2) = missingVals

modString = rminizinc:::set_params(modData = pVals2, mznpath = mznName, modify_mzn = TRUE)

# check if there are any missing parameters
missingVals = rminizinc:::getMissingPars(mznpath = mznName)

assert_true(length(missingVals) == 0)

# get the solutions
solution = rminizinc:::mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                     libpath = "/snap/minizinc/current/share/minizinc")

# get the optimal solution
print(solution$Solutions$optimal_solution)

