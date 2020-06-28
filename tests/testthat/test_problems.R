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
  expect_equal(parseInfo$Constraints$varsInvolved$`constraint: 0`, c("JOB", "m", "s", "d", "s"))
  expect_equal(parseInfo$Constraints$varsInvolved$`constraint: 1`, c("JOB", "TASK", "s", "d", "s", "d"))
  expect_equal(parseInfo$SolveType$objective, "minimize");
  expect_equal(parseInfo$SolveType$varsInvolved, "makespan")
  expect_equal(parseInfo$functionInformation$noOfFunctions, 1)
  expect_equal(parseInfo$functionInformation$functionDetails$`function: 0`$fnName, "nonoverlap")
  expect_equal(parseInfo$functionInformation$functionDetails$`function: 0`$varsInvolved[[1]], 
               c("s1", "d1", "s2", "s2", "d2", "s1"))
  
  missingPars = getMissingPars(mznpath = mznName)
  expect_equal(missingPars, c("n", "m", "d", "mc"))
  
  pVals = list(3, 4, matrix(c(3,3,4,4,4,3,2,2,3,3,3,4), nrow = 3, ncol = 4, byrow = TRUE),
               matrix(c(1,2,3,4,1,3,2,4,4,2,1,3), nrow=3, ncol=4, byrow = TRUE))
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
  
  expect_length(parseObj$Constraints$varsInvolved$'constraint: 0',3)
  
  #check if correct constraints are parse
  expect_equal(parseObj$Constraints$varsInvolved$'constraint: 0', c("Resources", "Products", "consumption"))
  
  expect_equal(parseObj$Constraints$varsInvolved$'constraint: 1', c("Resources", "used", "Products",
                                                                    "consumption", "produce", "used", "capacity"))
  
  # check if solve type and variables in the maximization or minimization expression are correct
  expect_equal(parseObj$SolveType$objective, "maximize")
  
  expect_equal(parseObj$SolveType$varsInvolved, c("Products", "profit", "produce"))
  
  # get the names of missing parameters
  missingNames = getMissingPars(mznpath = mznName)
  
  # set the values of these missing parameters
  pVals = list(2,c(400, 500), c("banana-cake", "chocolate-cake"), 5, c(4000, 6, 2000, 500, 500),
               c("flour","banana","sugar","butter","cocoa"), 
               matrix(c(250, 2, 75, 100, 0, 200, 0, 150, 150, 75), nrow = 2, ncol = 5, byrow = TRUE))
  
  names(pVals) = missingNames
  
  modString = set_params(modData = pVals, mznpath= mznName)
  
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
  expect_equal(parseObj$SolveType$varsInvolved, c("COD", "profit"))
  
  missingPars = getMissingPars(mznpath = mznName)
  expect_equal(missingPars, c("n", "m", "profit"))
  
  pVals = list(4, 4, matrix(c(7, 1, 3, 4, 8, 2, 5, 1, 4, 3, 7, 2, 3, 1, 6, 3), nrow = 4, ncol = 4, byrow = TRUE))
  names(pVals) = missingPars
  
  modString = set_params(modData = pVals, mznpath = mznName, modify_mzn = FALSE)
  
  solution  = mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                       libpath = "/snap/minizinc/current/share/minizinc")
  
  expect_equal(solution$Solutions$optimal_solution$task, list(4,1,2,3))
  expect_equal(solution$Solutions$optimal_solution$worker, list(2, 3, 4, 1))
})
