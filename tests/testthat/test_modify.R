test_that("The domain can be modified correctly",{
  # model string
  modString = "int: n;"
  # domain can't be modified if it doesn't exist
  expect_error(modifyDomainId(ItemNo = 0, modelString = modString))
  
  modString = "set of int: n; array [1..n] of var 0..1: testVar;"
  
  updatedModString = modifyDomainId(ItemNo = 1, maxIdItem = 0, modelString = modString)
  parseObj = mzn_parse(modelString = updatedModString)
  expect_equal(parseObj$VARIABLES$DECL2$DETAILS$DOMAIN$BINARY_OPERATION$LHS$INT, 0)
  expect_equal(parseObj$VARIABLES$DECL2$DETAILS$DOMAIN$BINARY_OPERATION$BINARY_OPERATOR, "DOTDOT")
  expect_equal(parseObj$VARIABLES$DECL2$DETAILS$DOMAIN$BINARY_OPERATION$RHS$ID, "n")
  
  updatedModString = modifyDomainSetVal(ItemNo = 1, imax = 2, imin = 0, modelString = modString)
  parseObj = mzn_parse(modelString = updatedModString)
  expect_equal(parseObj$VARIABLES$DECL2$DETAILS$DOMAIN$SET, c(MAX = 2, MIN = 0))
  
  updatedModString = modifyDomainFnCall(ItemNo = 1, maxIdItem = 0 , maxFnName = "max", modelString = modString)
  parseObj = mzn_parse(modelString = updatedModString)
  expect_equal(parseObj$VARIABLES$DECL2$DETAILS$DOMAIN$BINARY_OPERATION$LHS$INT, 1)
  expect_equal(parseObj$VARIABLES$DECL2$DETAILS$DOMAIN$BINARY_OPERATION$BINARY_OPERATOR, "DOTDOT")
  expect_equal(parseObj$VARIABLES$DECL2$DETAILS$DOMAIN$BINARY_OPERATION$RHS$FUNCTION_CALL$NAME, "max")
  expect_equal(parseObj$VARIABLES$DECL2$DETAILS$DOMAIN$BINARY_OPERATION$RHS$FUNCTION_CALL$ARGUMENTS$ARG1$ID, "n")
  
  
  modString = "set of int: n; array [1..n] of var 0..n: testVar;"
  updatedModString = modifyDomainAO(ItemNo = 1, minVal = 1, maxVal = 1 ,OPmax ='MULT', OPmin = "MULT", modelString = modString)
  parseObj = mzn_parse(modelString = updatedModString)
  expect_equal(parseObj$VARIABLES$DECL2$DETAILS$DOMAIN$BINARY_OPERATION$LHS$BINARY_OPERATION$LHS$INT, 0)
  expect_equal(parseObj$VARIABLES$DECL2$DETAILS$DOMAIN$BINARY_OPERATION$LHS$BINARY_OPERATION$BINARY_OPERATOR, "MULTIPLY")
  expect_equal(parseObj$VARIABLES$DECL2$DETAILS$DOMAIN$BINARY_OPERATION$LHS$BINARY_OPERATION$RHS$INT, 1)
  expect_equal(parseObj$VARIABLES$DECL2$DETAILS$DOMAIN$BINARY_OPERATION$RHS$BINARY_OPERATION$LHS$ID, "n")
  expect_equal(parseObj$VARIABLES$DECL2$DETAILS$DOMAIN$BINARY_OPERATION$RHS$BINARY_OPERATION$BINARY_OPERATOR, "MULTIPLY")
  expect_equal(parseObj$VARIABLES$DECL2$DETAILS$DOMAIN$BINARY_OPERATION$RHS$BINARY_OPERATION$RHS$INT, 1)
})