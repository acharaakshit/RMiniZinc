library(rminizinc)
library(checkmate)

# give a file name to read the mzn and write the updated mzn
mznName = "mzn_test_examples/knapsack_0_update.mzn"

# parse the model
obj1 <- rminizinc:::mzn_parse(mznpath = mznName)

# get the missing parameters
missingVals = obj1$missingValues

print(missingVals)

# give values to some of the parameters
pVals1 = list(3,9)
names(pVals1) = c(missingVals[1:2])

# set the parameters
rminizinc:::set_params(modData = pVals1, mznpath = mznName)

# find the missing parameters again
obj2 = rminizinc:::mzn_parse(mznpath = mznName)

print(obj2$missingValues)

# test that the missing values are only capacity and size now
assert(test_set_equal(obj2$missingValues, c("profit", "size")))

# give values to all the missing parameters
pVals2 = list(c(15,10,7),c(4,3,2))
names(pVals2) = obj2$missingValues

modString = rminizinc:::set_params(modData = pVals2, mznpath = mznName)

# check if there are any missing parameters
obj3 = rminizinc:::mzn_parse(mznpath = mznName)

assert_true(length(obj3$missingValues) == 0)

# get the solutions
solution = rminizinc:::mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                     libpath = "/snap/minizinc/current/share/minizinc")

# get the optimal solution
print(solution$Solutions$optimal_solution)

