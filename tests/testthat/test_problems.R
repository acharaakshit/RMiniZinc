skip_if_no_libminizinc = function(){
  data("config")
  data("slvbin")
  if(LIBMINIZINC_PATH == "" || SOLVER_BIN == ""){
    return(0)
  }
  return(1)
}

test_that("type compatibility is detected correctly",{
  if(skip_if_no_libminizinc()){
    expect_error(set_params(modData = 1, modify_mzn = "A"))
    expect_error(mzn_parse(mznpath = 1))
    expect_error(deleteItem(itemNo = 'a'))  
  }
})

test_that("production planning problems are solved",{
  data("config")
  if(skip_if_no_libminizinc()){
  # for devtools::test()
  mznName = "../../inst/extdata/mzn_examples/production_planning/prod_plan_0.mzn"
  
  if(stringr::str_detect(getwd(), c("rminizinc.Rcheck")) && !stringr::str_detect(getwd(), c("RMiniZinc"))){
    # for R CMD CHECK
    mznName = paste0(dirname(dirname(dirname(getwd()))), "/RMiniZinc/inst/extdata/mzn_examples/production_planning/prod_plan_0.mzn")
  }else if(stringr::str_detect(getwd(), c("rminizinc.Rcheck")) && stringr::str_detect(getwd(), c("RMiniZinc"))){
    # for travis build
    mznName = paste0(dirname(dirname(dirname(getwd()))), "/inst/extdata/mzn_examples/production_planning/prod_plan_0.mzn") 
  }
  
  parseInfo = mzn_parse(mzn_path = mznName)
  
  missingPars = get_missing_pars(parseInfo)
  expect_equal(missingPars, c("nproducts", "profit", "pname",
                              "nresources", "capacity", "rname",
                                "consumption"))
  
  pVals = list(Int$new(2), Array$new(exprVec = c(Int$new(400), Int$new(450))), 
               Array$new(exprVec = c(String$new("banana-cake"), String$new("chocolate-cake"))),
               Int$new(5), 
               Array$new(exprVec = intExpressions(c(4000, 6, 2000, 500, 500))),
               Array$new(exprVec = stringExpressions(c("flour","banana","sugar","butter","cocoa"))),
               Array$new(exprVec = intExpressions(c(250, 2, 75, 100, 0, 200, 0, 150, 150, 75)),
                         dimranges = c(IntSetVal$new(1,2), IntSetVal$new(1,5))))
  names(pVals) = missingPars
  
  model = set_params(modData = pVals, model = parseInfo)
  data("proot")
  solution  = mzn_eval(r_model = model, solver = "org.gecode.gecode",
                       lib_path = paste0(PROJECT_DIRECTORY, "/inst/minizinc/"))

  expect_equal(solution$SOLUTIONS$OPTIMAL_SOLUTION$produce, c(2, 2))
  expect_equal(solution$SOLUTIONS$OPTIMAL_SOLUTION$used, c( 900, 4, 450, 500, 150))
  }
})
