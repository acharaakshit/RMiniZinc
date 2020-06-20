# NOT--SUPPORTED--CURRENTLY


library(rminizinc)
library(checkmate)

# give a file path to read the mzn and write the updated mzn
mznName = "mzn_test_examples/production_planning/prod_plan_0_update.mzn"

# parse the model
obj1 <- rminizinc:::mzn_parse(mznpath = mznName)

# get the missing parameters
missingVals = obj1$missingValues

print(missingVals)

# give values to some of the parameters
pVals1 = list(2,c(400, 500))
names(pVals1) = c(missingVals[1:2])

# set the parameters
rminizinc:::set_params(modData = pVals1, mznpath = mznName)

# find the missing parameters again
obj2 = rminizinc:::mzn_parse(mznpath = mznName)

print(obj2$missingValues)

# test that the missing values are only capacity and size now
assert(test_set_equal(obj2$missingValues, c("pname", "nresources", "capacity", "rname", "consumption")))

# give values to all the missing parameters
pVals2 = list(c("banana-cake", "chocolate-cake"),5, c(4000, 6, 2000, 500, 500))
names(pVals2) = obj2$missingValues[1:3]

rminizinc:::set_params(modData = pVals2, mznpath = mznName)

# check if there are any missing parameters
obj3 = rminizinc:::mzn_parse(mznpath = mznName)

assert(test_set_equal(obj3$missingValues, c("rname", "consumption")))

pVals3 = list(c("flour","banana","sugar","butter","cocoa"), list(c(250, 2, 75, 100, 0), c(200, 0, 150, 150, 75)))
names(pVals3) = obj3$missingValues

modString = rminizinc:::set_params(modData = pVals3, mznpath = mznName)

obj4 = rminizinc:::mzn_parse(mznpath = mznName)

assert_true(length(obj4$missingValues) == 0)

# get the solutions
solution = rminizinc:::mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                                libpath = "/snap/minizinc/current/share/minizinc")

# get the optimal solution
print(solution$Solutions$optimal_solution)

