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
