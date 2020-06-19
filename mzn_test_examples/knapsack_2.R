library(rminizinc)
library(checkmate)

# give a file name to read the mzn and write the updated mzn
mznName = "mzn_test_examples/knapsack_2_update.mzn"

# parse the model
obj1 <- rminizinc:::mzn_parse(mznpath = mznName)

# get the missing parameters
missingVals = obj1$missingValues

print(missingVals)

# give values to some of the parameters
pVals1 = list(5,200)
names(pVals1) = c(missingVals[1:2])

# set the parameters
rminizinc:::set_params(modData = pVals1, mznpath = mznName)

# find the missing parameters again
obj2 = rminizinc:::mzn_parse(mznpath = mznName)

print(obj2$missingValues)

# test that the missing values are only profit and size now
assert(test_set_equal(obj2$missingValues, c("profit", "size")))

# give values to one of the missing parameters
pVals2 = list(c(1300,1000,520,480,325))
names(pVals2) = obj2$missingValues[1]

rminizinc:::set_params(modData = pVals2, mznpath = mznName)

# get the  missing parameters
obj3 = rminizinc:::mzn_parse(mznpath = mznName)

# test that the missing values is size now
assert(obj3$missingValues == "size")

#give values to the missing parameters
pVals3 = list(c(90,72,43,40,33))
names(pVals3) = obj3$missingValues

modString = rminizinc:::set_params(modData = pVals3, mznpath = mznName)

# check if there are any missing parameters
obj4 = rminizinc:::mzn_parse(mznpath = mznName)

assert_true(length(obj4$missingValues) == 0)

# get the solutions
solution = rminizinc:::mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                                libpath = "/snap/minizinc/current/share/minizinc")

# get the optimal solution
print(solution$Solutions$optimal_solution)

