library(stringr)

test_that("workflow is implemented correctly",{
  # for devtools::test()
  mznName = "../../mzn_test_examples/production_planning/prod_plan_0_update.mzn"
  
  if(str_detect(getwd(), c("rminizinc.Rcheck")) && !str_detect(getwd(), c("RMiniZinc"))){
    # for R CMD CHECK
    mznName = paste0(dirname(dirname(dirname(getwd()))), "/RMiniZinc/mzn_test_examples/production_planning/prod_plan_0_update.mzn")
  }else if(str_detect(getwd(), c("rminizinc.Rcheck")) && str_detect(getwd(), c("RMiniZinc"))){
    # for travis build
    mznName = paste0(dirname(dirname(dirname(getwd()))), "/mzn_test_examples/production_planning/prod_plan_0_update.mzn") 
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