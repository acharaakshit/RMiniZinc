library(stringr) 
test_that("type compatibility is detected correctly",{
  expect_error(set_params(modData = 1, modify_mzn = "A"))
  expect_error(mzn_parse(mznpath = 1))
  expect_error(deleteItem(itemNo = 'a'))
})

test_that("jobshop problems are solved without issues",{
  # for devtools::test()
  mznName = "../../inst/extdata/mzn_examples/jobshop/jobshop_0.mzn"
  
  if(str_detect(getwd(), c("rminizinc.Rcheck")) && !str_detect(getwd(), c("RMiniZinc"))){
    # for R CMD CHECK
    mznName = paste0(dirname(dirname(dirname(getwd()))), "/RMiniZinc/inst/extdata/mzn_examples/jobshop/jobshop_0.mzn")
  }else if(str_detect(getwd(), c("rminizinc.Rcheck")) && str_detect(getwd(), c("RMiniZinc"))){
    # for travis build
    mznName = paste0(dirname(dirname(dirname(getwd()))), "/inst/extdata/mzn_examples/jobshop/jobshop_0.mzn") 
  }
  
  parseInfo <- mzn_parse(mznpath = mznName)
  expect_equal(length(parseInfo$VARIABLES), 10)
  nVars = length(parseInfo$VARIABLES)
  v = c()
  k = c()
  t = c()
  for(i in seq(1, nVars, 1)){
    nDecl = as.symbol(paste0("DECL",i))
    v = c(v, parseInfo$VARIABLES[[nDecl]][["DETAILS"]][["NAME"]])
    k = c(k , parseInfo$VARIABLES[[nDecl]][["DETAILS"]][["KIND"]])
    t = c(t, parseInfo$VARIABLES[[nDecl]][["DETAILS"]][["TYPE"]])
  }
  expect_equal(v, c("n", "JOB", "m", "MACH", "TASK", "d", "mc", "maxt", "s", "makespan"))
  expect_equal(k, c("par", "par", "par", "par", "par", "par", "par",
                    "par", "var", "var"))
  expect_equal(t, c("int","set of int", "int", "set of int", "set of int", "2 dimensional array of int",
                    "2 dimensional unknown array","int", "2 dimensional unknown array",
                    "type couldn't be  identified"))

  expect_length(parseInfo$CONSTRAINTS, 2)
  
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
  mznName = "../../inst/extdata/mzn_examples/production_planning/prod_plan_0.mzn"
  
  if(str_detect(getwd(), c("rminizinc.Rcheck")) && !str_detect(getwd(), c("RMiniZinc"))){
    # for R CMD CHECK
    mznName = paste0(dirname(dirname(dirname(getwd()))), "/RMiniZinc/inst/extdata/mzn_examples/production_planning/prod_plan_0.mzn")
  }else if(str_detect(getwd(), c("rminizinc.Rcheck")) && str_detect(getwd(), c("RMiniZinc"))){
    # for travis build
    mznName = paste0(dirname(dirname(dirname(getwd()))), "/inst/extdata/mzn_examples/production_planning/prod_plan_0.mzn") 
  }
  
  parseInfo = mzn_parse(mznpath = mznName)
  expect_equal(length(parseInfo$VARIABLES), 12)
  nVars = length(parseInfo$VARIABLES)
  v = c()
  k = c()
  t = c()
  for(i in seq(1, nVars, 1)){
    nDecl = as.symbol(paste0("DECL",i))
    v = c(v, parseInfo$VARIABLES[[nDecl]][["DETAILS"]][["NAME"]])
    k = c(k , parseInfo$VARIABLES[[nDecl]][["DETAILS"]][["KIND"]])
    t = c(t, parseInfo$VARIABLES[[nDecl]][["DETAILS"]][["TYPE"]])
  }
  
  expect_equal(v, c("nproducts", "Products", "profit", "pname", "nresources",
                    "Resources", "capacity", "rname", "consumption", "mproducts",
                    "produce", "used"))
  expect_equal(k, c("par", "par", "par", "par", "par", "par", "par",
                    "par", "par", "par", "var", "var"))
  expect_equal(t, c("int", "set of int", "1 dimensional array of int", "1 dimensional array of string", "int",
                    "set of int", "1 dimensional array of int", "1 dimensional array of string",
                    "2 dimensional array of int", "int" ,"1 dimensional unknown array", "1 dimensional unknown array"))
  
  
  expect_length(parseInfo$CONSTRAINTS,2)

  
  # check if solve type and variables in the maximization or minimization expression are correct
  expect_equal(parseInfo$SOLVE_TYPE$OBJECTIVE, "maximize")
  
  # get the names of missing parameters
  missingnames = getMissingPars(mznpath = mznName)
  
  # set the values of these missing parameters
  pVals = list(2,c(400, 500), c("banana-cake", "chocolate-cake"), 5, c(4000, 6, 2000, 500, 500),
               c("flour","banana","sugar","butter","cocoa"), 
               c(250, 2, 75, 100, 0, 200, 0, 150, 150, 75))
  
  names(pVals) = missingnames
  
  modString = set_params(modData = pVals, mznpath= mznName, modify_mzn = FALSE)
  
  expect_length(getMissingPars(modelString = modString), 0)
  
  # evaluate the function and get the solutions
  solution = mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                      libpath = "/snap/minizinc/current/share/minizinc")
  
  expect_length(solution$SOLUTIONS, 7)
  expect_length(solution$SOLUTIONS$OPTIMAL_SOLUTION$produce, 2)
  expect_equal(solution$SOLUTIONS$OPTIMAL_SOLUTION$produce, c(2,2))
  expect_length(solution$SOLUTIONS$OPTIMAL_SOLUTION$used, 5)
  expect_equal(solution$SOLUTIONS$OPTIMAL_SOLUTION$used, c(900, 4, 450, 500, 150))
})

test_that("assignment problems can be solved", {
  
  # for devtools::test()
  mznName = "../../inst/extdata/mzn_examples/assign/assign_inverse.mzn"
  
  if(str_detect(getwd(), c("rminizinc.Rcheck")) && !str_detect(getwd(), c("RMiniZinc"))){
    # for R CMD CHECK
    mznName = paste0(dirname(dirname(dirname(getwd()))), "/RMiniZinc/inst/extdata/mzn_examples/assign/assign_inverse.mzn")
  }else if(str_detect(getwd(), c("rminizinc.Rcheck")) && str_detect(getwd(), c("RMiniZinc"))){
    # for travis build
    mznName = paste0(dirname(dirname(dirname(getwd()))), "/inst/extdata/mzn_examples/assign/assign_inverse.mzn") 
  }
  
  parseInfo = mzn_parse(mznpath = mznName)
  expect_equal(length(parseInfo$VARIABLES), 7)
  nVars = length(parseInfo$VARIABLES)
  v = c()
  k = c()
  t = c()
  for(i in seq(1, nVars, 1)){
    nDecl = as.symbol(paste0("DECL",i))
    v = c(v, parseInfo$VARIABLES[[nDecl]][["DETAILS"]][["NAME"]])
    k = c(k , parseInfo$VARIABLES[[nDecl]][["DETAILS"]][["KIND"]])
    t = c(t, parseInfo$VARIABLES[[nDecl]][["DETAILS"]][["TYPE"]])
  }
  expect_equal(v, c("n", "DOM", "m", "COD", "profit", "task", "worker"))
  expect_equal(k, c("par", "par", "par", "par", "par",
                    "var", "var"))
  expect_equal(t, c("int", "set of int", "int", "set of int", "2 dimensional array of int",
                    "1 dimensional unknown array", "1 dimensional unknown array"))
  
  expect_equal(parseInfo$INCLUDES$INCLUDE1$INCLUDED_MZN, "inverse.mzn")
  expect_equal(parseInfo$SOLVE_TYPE$OBJECTIVE, "maximize")
  
  missingPars = getMissingPars(mznpath = mznName)
  expect_equal(missingPars, c("n", "m", "profit"))
  
  pVals = list(4, 4, c(7, 1, 3, 4, 8, 2, 5, 1, 4, 3, 7, 2, 3, 1, 6, 3))
  names(pVals) = missingPars
  
  modString = set_params(modData = pVals, mznpath = mznName, modify_mzn = FALSE)
  
  solution  = mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                       libpath = "/snap/minizinc/current/share/minizinc")
  
  expect_equal(solution$SOLUTIONS$OPTIMAL_SOLUTION$task, c(4,1,2,3))
  expect_equal(solution$SOLUTIONS$OPTIMAL_SOLUTION$worker, c(2, 3, 4, 1))
})


test_that("crazy set problems can be solved", {
  # for devtools::test()
  mznName = "../../inst/extdata/mzn_examples/crazy_sets/crazy_sets.mzn"
  
  if(str_detect(getwd(), c("rminizinc.Rcheck")) && !str_detect(getwd(), c("RMiniZinc"))){
    # for R CMD CHECK
    mznName = paste0(dirname(dirname(dirname(getwd()))), "/RMiniZinc/inst/extdata/mzn_examples/crazy_sets/crazy_sets.mzn")
  }else if(str_detect(getwd(), c("rminizinc.Rcheck")) && str_detect(getwd(), c("RMiniZinc"))){
    # for travis build
    mznName = paste0(dirname(dirname(dirname(getwd()))), "/inst/extdata/mzn_examples/crazy_sets/crazy_sets.mzn") 
  }
  
  parseInfo = mzn_parse(mznpath = mznName)
  expect_equal(length(parseInfo$VARIABLES), 6)
  nVars = length(parseInfo$VARIABLES)
  v = c()
  k = c()
  t = c()
  for(i in seq(1, nVars, 1)){
    nDecl = as.symbol(paste0("DECL",i))
    v = c(v, parseInfo$VARIABLES[[nDecl]][["DETAILS"]][["NAME"]])
    k = c(k , parseInfo$VARIABLES[[nDecl]][["DETAILS"]][["KIND"]])
    t = c(t, parseInfo$VARIABLES[[nDecl]][["DETAILS"]][["TYPE"]])
  }
  expect_equal(v,c("n", "NUMBER", "c", "m", "s", "x"))
  expect_equal(k, c("par", "par", "par", "par", "var",
               "var"))
  expect_equal(t, c("int", "set of int", "int", "int", "1 dimensional unknown array",
                    "2 dimensional unknown array"))
  expect_equal(parseInfo$CONSTRAINTS$CONSTRAINT1$DETAILS$FUNCTION_CALL$NAME, "forall")
  
  expect_equal(parseInfo$CONSTRAINTS$CONSTRAINT1$DETAILS$FUNCTION_CALL$ARGUMENTS$ARG1$COMPREHENSION$GENERATORS$GENERATOR1$DECLARATIONS$DECL1$VARIABLE_DECLARATION$NAME, "i")
  expect_equal(parseInfo$CONSTRAINTS$CONSTRAINT1$DETAILS$FUNCTION_CALL$ARGUMENTS$ARG1$COMPREHENSION$GENERATORS$GENERATOR1$DECLARATIONS$DECL1$VARIABLE_DECLARATION$KIND, "par")
  expect_equal(parseInfo$CONSTRAINTS$CONSTRAINT1$DETAILS$FUNCTION_CALL$ARGUMENTS$ARG1$COMPREHENSION$GENERATORS$GENERATOR1$DECLARATIONS$DECL1$VARIABLE_DECLARATION$TYPE, "int")
  
  expect_equal(parseInfo$CONSTRAINTS$CONSTRAINT1$DETAILS$FUNCTION_CALL$ARGUMENTS$ARG1$COMPREHENSION$GENERATORS$GENERATOR1$DECLARATIONS$DECL2$VARIABLE_DECLARATION$NAME, "j")
  expect_equal(parseInfo$CONSTRAINTS$CONSTRAINT1$DETAILS$FUNCTION_CALL$ARGUMENTS$ARG1$COMPREHENSION$GENERATORS$GENERATOR1$DECLARATIONS$DECL2$VARIABLE_DECLARATION$KIND, "par")
  expect_equal(parseInfo$CONSTRAINTS$CONSTRAINT1$DETAILS$FUNCTION_CALL$ARGUMENTS$ARG1$COMPREHENSION$GENERATORS$GENERATOR1$DECLARATIONS$DECL2$VARIABLE_DECLARATION$TYPE, "int")
  
  expect_equal(parseInfo$CONSTRAINTS$CONSTRAINT1$DETAILS$FUNCTION_CALL$ARGUMENTS$ARG1$COMPREHENSION$GENERATORS$GENERATOR1$DECLARATIONS$DECL3$VARIABLE_DECLARATION$NAME, "k")
  expect_equal(parseInfo$CONSTRAINTS$CONSTRAINT1$DETAILS$FUNCTION_CALL$ARGUMENTS$ARG1$COMPREHENSION$GENERATORS$GENERATOR1$DECLARATIONS$DECL3$VARIABLE_DECLARATION$KIND, "par")
  expect_equal(parseInfo$CONSTRAINTS$CONSTRAINT1$DETAILS$FUNCTION_CALL$ARGUMENTS$ARG1$COMPREHENSION$GENERATORS$GENERATOR1$DECLARATIONS$DECL3$VARIABLE_DECLARATION$TYPE, "int")
  
  expect_equal(parseInfo$CONSTRAINTS$CONSTRAINT1$DETAILS$FUNCTION_CALL$ARGUMENTS$ARG1$COMPREHENSION$GENERATORS$GENERATOR1$IN$BINARY_OPERATION$LHS$INT, 1)
  expect_equal(parseInfo$CONSTRAINTS$CONSTRAINT1$DETAILS$FUNCTION_CALL$ARGUMENTS$ARG1$COMPREHENSION$GENERATORS$GENERATOR1$IN$BINARY_OPERATION$BINARY_OPERATOR, "'..'")
  expect_equal(parseInfo$CONSTRAINTS$CONSTRAINT1$DETAILS$FUNCTION_CALL$ARGUMENTS$ARG1$COMPREHENSION$GENERATORS$GENERATOR1$IN$BINARY_OPERATION$RHS$ID, "m")
  
  expect_equal(parseInfo$CONSTRAINTS$CONSTRAINT1$DETAILS$FUNCTION_CALL$ARGUMENTS$ARG1$COMPREHENSION$GENERATORS$GENERATOR1$WHERE$BINARY_OPERATION$LHS$BINARY_OPERATION$LHS$ID, "i")
  expect_equal(parseInfo$CONSTRAINTS$CONSTRAINT1$DETAILS$FUNCTION_CALL$ARGUMENTS$ARG1$COMPREHENSION$GENERATORS$GENERATOR1$WHERE$BINARY_OPERATION$LHS$BINARY_OPERATION$BINARY_OPERATOR, "'<'")
  expect_equal(parseInfo$CONSTRAINTS$CONSTRAINT1$DETAILS$FUNCTION_CALL$ARGUMENTS$ARG1$COMPREHENSION$GENERATORS$GENERATOR1$WHERE$BINARY_OPERATION$LHS$BINARY_OPERATION$RHS$ID, "j")

  expect_equal(parseInfo$CONSTRAINTS$CONSTRAINT1$DETAILS$FUNCTION_CALL$ARGUMENTS$ARG1$COMPREHENSION$EXPRESSION$BINARY_OPERATION$LHS$BINARY_OPERATION$BINARY_OPERATOR, "'intersect'")    
  
  expect_equal(parseInfo$CONSTRAINTS$CONSTRAINT1$DETAILS$FUNCTION_CALL$ARGUMENTS$ARG1$COMPREHENSION$EXPRESSION$BINARY_OPERATION$LHS$BINARY_OPERATION$RHS$ARRAY_ACCESS$NAME$ID, "s")
  expect_equal(parseInfo$CONSTRAINTS$CONSTRAINT1$DETAILS$FUNCTION_CALL$ARGUMENTS$ARG1$COMPREHENSION$EXPRESSION$BINARY_OPERATION$LHS$BINARY_OPERATION$RHS$ARRAY_ACCESS$ARGUMENTS$ARG1$ID, "k")
  
  expect_equal(parseInfo$CONSTRAINTS$CONSTRAINT1$DETAILS$FUNCTION_CALL$ARGUMENTS$ARG1$COMPREHENSION$EXPRESSION$BINARY_OPERATION$BINARY_OPERATOR, "'='")
  expect_equal(length(parseInfo$CONSTRAINTS$CONSTRAINT1$DETAILS$FUNCTION_CALL$ARGUMENTS$ARG1$COMPREHENSION$EXPRESSION$BINARY_OPERATION$RHS$SET), 0)
  
  
  expect_equal(parseInfo$SOLVE_TYPE$OBJECTIVE, "satisfy")
  
 
  missingPars = getMissingPars(mznpath = mznName)
  
  pVals = list(10, 4, 4)
  names(pVals) = missingPars
  
  modString = set_params(modData = pVals, mznpath = mznName, modify_mzn = FALSE)
  
  solution = mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                      libpath = "/snap/minizinc/current/share/minizinc",
                      all_solutions = FALSE)
  
})

