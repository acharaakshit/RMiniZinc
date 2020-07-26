test_that("The domain can be modified correctly",{
  # model string
  modString = "int: n;"
  # domain can't be modified if it doesn't exist
  expect_error(modifyDomainId(ItemNo = 0, modelString = modString))
  
  modString = "set of int: n; array [1..n] of var 0..1: testVar; constraint n < 10;"
  
  updatedModString = modifyDomainId(ItemNo = 1, maxIdItem = 0, modelString = modString)
  parseObj = mzn_parse(modelString = updatedModString)
  expect_equal(parseObj$Variables$decl2$domain, list(min = 0, max ="n"))
  
  updatedModString = modifyDomainSetVal(ItemNo = 1, imax = 2, imin = 0, modelString = modString)
  parseObj = mzn_parse(modelString = updatedModString)
  expect_equal(parseObj$Variables$decl2$domain[[1]], c(max = 2, min= 0))
  
  updatedModString = modifyDomainFnCall(ItemNo = 1, maxIdItem = 0 ,maxFnName = "max", modelString = modString)
  parseObj = mzn_parse(modelString = modString)
  expect_equal(parseObj$Variables$decl2$domain[[1]], c(max = 1, min= 0))
  
  modString = "set of int: n; array [1..n] of var 0..n: testVar; constraint n < 10;"
  updatedModString = modifyDomainAO(ItemNo = 1, minVal = 1, maxVal = 1 ,OPmax ='MULT', OPmin = "MULT", modelString = modString)
  parseObj = mzn_parse(modelString = modString)
  print(parseObj)
  expect_equal(parseObj$Variables$decl2$domain$min, 0)
  expect_equal(parseObj$Variables$decl2$domain$max, "n")

})