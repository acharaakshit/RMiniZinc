library(rminizinc)
library(checkmate)

# give a file path to read the mzn and write the updated mzn
mznName = "mzn_test_examples/knapsack/knapsack_4_update.mzn"
# mznName = "mzn_test_examples/knapsack/knapsack_2_update.mzn"
# mznName = "mzn_test_examples/knapsack/knapsack_3_update.mzn"

# parse the model
missingVals = rminizinc:::getMissingPars(mznpath = mznName)

# give values to some of the parameters
pVals1 = list(5,200)
names(pVals1) = c(missingVals[1:2])

# set the parameters
rminizinc:::set_params(modData = pVals1, mznpath = mznName, modify_mzn = TRUE)

# find the missing parameters again
missingVals = rminizinc:::getMissingPars(mznpath = mznName)

# test that the missing values are only profit and size now
assert(test_set_equal(missingVals, c("profit", "size")))

# give values to one of the missing parameters
pVals2 = list(c(1300,1000,520,480,325))
names(pVals2) = missingVals[1]

rminizinc:::set_params(modData = pVals2, mznpath = mznName, modify_mzn = TRUE)

# get the  missing parameters
missingVals = rminizinc:::getMissingPars(mznpath = mznName)

# test that the missing values is size now
assert(missingVals == "size")

#give values to the missing parameters
pVals3 = list(c(90,72,43,40,33))
names(pVals3) = missingVals

modString = rminizinc:::set_params(modData = pVals3, mznpath = mznName, modify_mzn = TRUE)

# check if there are any missing parameters
missingVals = rminizinc:::getMissingPars(mznpath = mznName)

assert_true(length(missingVals) == 0)

# get the solutions
solution = rminizinc:::mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                                libpath = "/snap/minizinc/current/share/minizinc")

# get the optimal solution
print(solution$Solutions$optimal_solution)

