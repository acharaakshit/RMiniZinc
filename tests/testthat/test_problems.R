library(stringr)

test_that("jobshop problems are solved without issues",{
  # for devtools::test()
  mznName = "../../mzn_examples/jobshop/jobshop_0.mzn"
  
  if(str_detect(getwd(), c("rminizinc.Rcheck")) && !str_detect(getwd(), c("RMiniZinc"))){
    # for R CMD CHECK
    mznName = paste0(dirname(dirname(dirname(getwd()))), "/RMiniZinc/mzn_examples/jobshop/jobshop_0.mzn")
  }else if(str_detect(getwd(), c("rminizinc.Rcheck")) && str_detect(getwd(), c("RMiniZinc"))){
    # for travis build
    mznName = paste0(dirname(dirname(dirname(getwd()))), "/mzn_examples/jobshop/jobshop_0.mzn") 
  }
  
  parseInfo <- mzn_parse(mznpath = mznName)
  expect_equal(parseInfo$Parameters, c("n", "JOB", "m", "MACH", "TASK", "d", "mc", "maxt"))
  expect_equal(parseInfo$decisionVariables, c("s", "makespan"))
  expect_length(parseInfo$Constraints, 2)
  expect_equal(parseInfo$Constraints$noOfConstraints, 2)
  expect_equal(parseInfo$Constraints$varsInvolved$`constraint: 0`, c("JOB", "d", "j", "m", "s", "t"))
  expect_equal(parseInfo$Constraints$varsInvolved$`constraint: 1`, c("JOB", "TASK", "d", "j1", "j2",
                                                                     "s", "t1", "t2"))
  expect_equal(parseInfo$SolveType$objective, "minimize");
  expect_equal(parseInfo$SolveType$varsInvolved, "makespan")
  expect_equal(parseInfo$functionInformation$noOfFunctions, 1)
  expect_equal(parseInfo$functionInformation$functionDetails$`function: 0`$fnName, "nonoverlap")
  expect_equal(parseInfo$functionInformation$functionDetails$`function: 0`$varsInvolved[[1]], 
               c("d1", "d2", "s1", "s2"))
  
  missingPars = getMissingPars(mznpath = mznName)
  expect_equal(missingPars, c("n", "m", "d", "mc"))
  
  pVals = list(3, 4, c(3,3,4,4,4,3,2,2,3,3,3,4),
               c(1,2,3,4,1,3,2,4,4,2,1,3))
  names(pVals) = missingPars
  
  modString = set_params(modData = pVals, mznpath = mznName, modify_mzn = FALSE)
  #print(modString)
  expect_length(getMissingPars(modelString = modString), 0)
  
  mzn_eval(solver = "org.gecode.gecode", libpath = "/snap/minizinc/current/share/minizinc",
           modelString = modString)  
})

test_that("production planning problems are solved",{
  # for devtools::test()
  mznName = "../../mzn_examples/production_planning/prod_plan_0_update.mzn"
  
  if(str_detect(getwd(), c("rminizinc.Rcheck")) && !str_detect(getwd(), c("RMiniZinc"))){
    # for R CMD CHECK
    mznName = paste0(dirname(dirname(dirname(getwd()))), "/RMiniZinc/mzn_examples/production_planning/prod_plan_0_update.mzn")
  }else if(str_detect(getwd(), c("rminizinc.Rcheck")) && str_detect(getwd(), c("RMiniZinc"))){
    # for travis build
    mznName = paste0(dirname(dirname(dirname(getwd()))), "/mzn_examples/production_planning/prod_plan_0_update.mzn") 
  }
  
  parseObj = mzn_parse(mznpath = mznName)
  # check if correct parameters are parsed 
  expect_equal(parseObj$Parameters, c("nproducts", "Products", "profit", "pname", "nresources",
                                      "Resources", "capacity", "rname", "consumption", "mproducts"))
  # check if correct decision variables are parsed
  expect_equal(parseObj$decisionVariables, c("produce", "used"))
  
  expect_length(parseObj$Constraints,2)
  
  expect_length(parseObj$Constraints$varsInvolved,2)
  
  #check if correct constraints are parse
  expect_equal(parseObj$Constraints$varsInvolved$'constraint: 0', c("Products", "Resources", "consumption",
                                                                    "p", "r"))
  
  expect_equal(parseObj$Constraints$varsInvolved$'constraint: 1', c("Products", "Resources", "capacity", 
                                                                    "consumption", "p", "produce",
                                                                    "r", "used" ))
  
  # check if solve type and variables in the maximization or minimization expression are correct
  expect_equal(parseObj$SolveType$objective, "maximize")
  
  expect_equal(parseObj$SolveType$varsInvolved, c("Products", "p", "produce", "profit"))
  
  # get the names of missing parameters
  missingNames = getMissingPars(mznpath = mznName)
  
  # set the values of these missing parameters
  pVals = list(2,c(400, 500), c("banana-cake", "chocolate-cake"), 5, c(4000, 6, 2000, 500, 500),
               c("flour","banana","sugar","butter","cocoa"), 
               c(250, 2, 75, 100, 0, 200, 0, 150, 150, 75))
  
  names(pVals) = missingNames
  
  modString = set_params(modData = pVals, mznpath= mznName, modify_mzn = FALSE)
  
  expect_length(getMissingPars(modelString = modString), 0)
  
  # evaluate the function and get the solutions
  solution = mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                      libpath = "/snap/minizinc/current/share/minizinc")
  
  expect_length(solution$Solutions, 7)
  expect_length(solution$Solutions$optimal_solution$produce, 2)
  expect_equal(solution$Solutions$optimal_solution$produce, list(2,2))
  expect_length(solution$Solutions$optimal_solution$used, 5)
  expect_equal(solution$Solutions$optimal_solution$used, list(900, 4, 450, 500, 150))
})

test_that("assignment problems are solved", {
  
  # for devtools::test()
  mznName = "../../mzn_examples/assign/assign_inverse.mzn"
  
  if(str_detect(getwd(), c("rminizinc.Rcheck")) && !str_detect(getwd(), c("RMiniZinc"))){
    # for R CMD CHECK
    mznName = paste0(dirname(dirname(dirname(getwd()))), "/RMiniZinc/mzn_examples/assign/assign_inverse.mzn")
  }else if(str_detect(getwd(), c("rminizinc.Rcheck")) && str_detect(getwd(), c("RMiniZinc"))){
    # for travis build
    mznName = paste0(dirname(dirname(dirname(getwd()))), "/mzn_examples/assign/assign_inverse.mzn") 
  }
  
  parseObj = mzn_parse(mznpath = mznName)
  expect_equal(parseObj$Parameters, c("n", "DOM", "m", "COD", "profit"))
  expect_equal(parseObj$decisionVariables, c("task", "worker"))
  expect_equal(parseObj$Constraints$noOfConstraints, 1)
  expect_equal(parseObj$Constraints$varsInvolved$`constraint: 0`, c("task", "worker"))
  expect_equal(parseObj$Includes, "inverse.mzn")
  expect_equal(parseObj$SolveType$objective, "maximize")
  expect_equal(parseObj$SolveType$varsInvolved, c("COD", "profit", "task", "w"))
  
  missingPars = getMissingPars(mznpath = mznName)
  expect_equal(missingPars, c("n", "m", "profit"))
  
  pVals = list(4, 4, c(7, 1, 3, 4, 8, 2, 5, 1, 4, 3, 7, 2, 3, 1, 6, 3))
  names(pVals) = missingPars
  
  modString = set_params(modData = pVals, mznpath = mznName, modify_mzn = FALSE)
  
  solution  = mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                       libpath = "/snap/minizinc/current/share/minizinc")
  
  expect_equal(solution$Solutions$optimal_solution$task, list(4,1,2,3))
  expect_equal(solution$Solutions$optimal_solution$worker, list(2, 3, 4, 1))
})

test_that("line travelling salesman problems can be solved", {
  # for devtools::test()
  mznName = "../../mzn_examples/linetsp/ltsp.mzn"
  
  if(str_detect(getwd(), c("rminizinc.Rcheck")) && !str_detect(getwd(), c("RMiniZinc"))){
    # for R CMD CHECK
    mznName = paste0(dirname(dirname(dirname(getwd()))), "/RMiniZinc/mzn_examples/linetsp/ltsp.mzn")
  }else if(str_detect(getwd(), c("rminizinc.Rcheck")) && str_detect(getwd(), c("RMiniZinc"))){
    # for travis build
    mznName = paste0(dirname(dirname(dirname(getwd()))), "/mzn_examples/linetsp/ltsp.mzn") 
  }
  
  parseObj = mzn_parse(mznpath = mznName)
  expect_equal(parseObj$Parameters, c("n", "CITY", "POS", "coord", "m", "PREC", "left", "right"))
  expect_equal(parseObj$decisionVariables, c("order", "city"))
  expect_equal(parseObj$Constraints$noOfConstraints, 2)
  expect_equal(parseObj$Constraints$varsInvolved$`constraint: 0`, c("city", "order"))
  expect_equal(parseObj$Constraints$varsInvolved$`constraint: 1`, c("PREC", "i", "left", "order", "right"))
  expect_equal(parseObj$SolveType$objective, "minimize")
  expect_equal(parseObj$SolveType$varsInvolved, c("city", "coord", "i", "n"))
  expect_equal(parseObj$Includes, "inverse.mzn")
  
  missingPars = getMissingPars(mznpath = mznName)
  
  pVals = list(8, c(-7, -4, -2, 0, 1, 6, 9, 12), 7, c(3, 7, 8, 3, 2, 1, 4),
               c(1, 2, 4, 6, 5, 7, 2))
  names(pVals) = missingPars
  
  modString = set_params(modData = pVals, mznpath = mznName, modify_mzn = FALSE)
  
  solution = mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                      libpath = "/snap/minizinc/current/share/minizinc")
  
  expect_equal(solution$Solutions$optimal_solution$order, list(2, 7, 1, 6, 8, 5, 4, 3))
  expect_equal(solution$Solutions$optimal_solution$city, list(3, 1, 8, 7, 6, 4, 2, 5))
  
})

test_that("crazy problems can be solved", {
  # for devtools::test()
  mznName = "../../mzn_examples/crazy_sets/crazy_sets.mzn"
  
  if(str_detect(getwd(), c("rminizinc.Rcheck")) && !str_detect(getwd(), c("RMiniZinc"))){
    # for R CMD CHECK
    mznName = paste0(dirname(dirname(dirname(getwd()))), "/RMiniZinc/mzn_examples/crazy_sets/crazy_sets.mzn")
  }else if(str_detect(getwd(), c("rminizinc.Rcheck")) && str_detect(getwd(), c("RMiniZinc"))){
    # for travis build
    mznName = paste0(dirname(dirname(dirname(getwd()))), "/mzn_examples/crazy_sets/crazy_sets.mzn") 
  }
  
  parseObj = mzn_parse(mznpath = mznName)
  expect_equal(parseObj$Parameters, c("n", "NUMBER", "c", "m"))
  expect_equal(parseObj$decisionVariables, c("s", "x"))
  expect_equal(parseObj$Constraints$noOfConstraints, 5)
  expect_equal(parseObj$Constraints$varsInvolved$`constraint: 0`, c("i", "j", "k", "m", "s"))
  expect_equal(parseObj$Constraints$varsInvolved$`constraint: 1`, c("c", "i", "j", "m", "x"))
  expect_equal(parseObj$Constraints$varsInvolved$`constraint: 2`, c("c", "i", "j", "m", "x"))
  expect_equal(parseObj$Constraints$varsInvolved$`constraint: 3`, c("NUMBER", "c",
                                                                    "i", "j", "m", "o", "s", "x"))
  expect_equal(parseObj$Constraints$varsInvolved$`constraint: 4`, c("c", "i", "j", "m", "s", "x"))
  
  expect_equal(parseObj$SolveType$objective, "satisfy")
 
  missingPars = getMissingPars(mznpath = mznName)
  
  pVals = list(10, 4, 4)
  names(pVals) = missingPars
  
  modString = set_params(modData = pVals, mznpath = mznName, modify_mzn = FALSE)
  
  solution = mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                      libpath = "/snap/minizinc/current/share/minizinc",
                      all_solutions = FALSE)
  
})

