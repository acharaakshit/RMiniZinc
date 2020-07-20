test_that("check if the variable declarations are correct",{
  
  # only integer value should be supplied to the IntVal classs
  expect_error(IntVal$new(4.3))
  # set values  -- currently only integer ranges are supported
  expect_error(SetVal$new(c(l=1.1, u=2.1)))
  # set can't be assigned an index
  expect_error(TypeInst$new(Type$new(base_type = "INT", kind = "parameter", set_type = TRUE),
                            indexExprVec = Set$new(SetVal$new(c(l=1, u=2)))))
})

test_that("'knapsack problems can be created",{
  # create the variable and parameter declarations
  
  par_ti = TypeInst$new(Type$new(base_type = "INT", kind = "parameter"))
  par1 = VarDecl$new(id = "n", par_ti, type_inst = par_ti)
  # create the Item 1
  item1 = VarDeclItem$new(decl = par1)
  
  sv = SetVal$new(val = c(l = 1, u=par1))
  par2_dt = Set$new(setVal = sv)
  par2_ti = TypeInst$new(Type$new(base_type = "INT", kind = "parameter", dim = 1, set_type = TRUE))
  par2 = VarDecl$new(type_inst = par2_ti, expression = par2_dt, id = "OBJ")
  # create the item 2
  item2 = VarDeclItem$new(decl = par2)
  
  par3_ti = TypeInst$new(Type$new(base_type = "INT", kind = "parameter"))
  par3 = VarDecl$new(type_inst = par3_ti, id = "capacity")
  # create the item 3
  item3 = VarDeclItem$new(decl = par3)
  
  par4_ti = TypeInst$new(Type$new(base_type = "INT", kind = "parameter", dim = 1),
                         indexExprVec = par2$id())
  par4 = VarDecl$new(type_inst = par4_ti, id = "profit")
  # create the item 4
  item4 = VarDeclItem$new(decl = par4)
  
  par5_ti = TypeInst$new(Type$new(base_type = "INT", kind = "parameter", dim = 1),
                         indexExprVec = par2$id())
  par5 = VarDecl$new(type = par5_ti, id = "size")
  # create the item 5
  item5 = VarDeclItem$new(decl = par5)
  
  par6_ti = TypeInst$new(Type$new(base_type = "INT", kind = "decision", dim = 1),
                         indexExprVec = par2$id())
  par6 = VarDecl$new(type = par6_ti, id = "x")
  #create the item 6
  item6 = VarDeclItem$new(decl = par6)
  
  # create the constraints
  
  # generator
  gen_forall = Generator$new(IN = par2$id(), iterator = "i")
  # binary operator expression
  bop1 = Binop$new(lhs_expression = ArrayAccess$new(id = par6$id(), index = gen_forall$iter_id()), binop = ">=", 
                   rhs_expression =    Int$new(value = IntVal$new(0)))
  # comprehension
  Comp1 = Comprehension$new(generators = list(gen_forall), expression = bop1)
  # forall function call
  cl1 = Call$new(fn_id = "forall", lExp = list(Comp1))
  # item7 
  item7 = ConstraintItem$new(expression = cl1)
  
  gen_sum = Generator$new(IN = par2$id(), iterator = "i")
  bop2 = Binop$new(lhs_expression = ArrayAccess$new(id = par5$id(), index = gen_sum$iter_id()), binop = "*", 
                   rhs_expression = ArrayAccess$new(id = par6$id() ,index = gen_sum$iter_id()))
  Comp2 = Comprehension$new(generators = list(gen_sum), expression = bop2)
  cl2 = Call$new(fn_id = "sum", lExp = list(Comp2))
  bop3 = Binop$new(lhs_expression = cl2, binop = "<=", rhs_expression = par3$id())
  # item8
  item8 = ConstraintItem$new(expression = bop3)
  
  # create solve type
  
  gen_sum = Generator$new(IN = par2$id(), iterator = "i")
  
  bop4 = Binop$new(lhs_expression = ArrayAccess$new(id = par4$id(),index = gen_sum$iter_id()), 
                   binop = "*", rhs_expression = ArrayAccess$new(id = par6$id(), index = gen_sum$iter_id()))
  
  Comp3 = Comprehension$new(generators = list(gen_sum), expression = bop4)
  
  cl3 = Call$new(fn_id = "sum", lExp = list(Comp3))
  
  # item9
  item9 = SolveItem$new(solve_type = "maximize", expression = cl3)
  
  # combine to create model
  
  items  = c(item1, item2, item3, item4, item5, item6, item7, item8, item9)
  mod = Model$new(items = items)
  modString = mod$mzn_string()
  pVals= list(3,9,c(15,10,7),c(4,3,2))
  names(pVals) = c(par1$id()$id(), par3$id()$id(), par4$id()$id(), par5$id()$id())
  
  modString = rminizinc:::set_params(modData = pVals, modelString = modString)
  
  sol = rminizinc:::mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                             libpath = "/snap/minizinc/current/share/minizinc")
  
  # there are 7 solutions
  expect_equal(length(sol$Solutions), 7)
  # check the value of the solutions
  expect_equal(sol$Solutions$optimal_solution$x, list(1,1,1))
  
  # check if parsed model is same 
  parsedModel = rminizinc:::mzn_parse(modelString = modString)
  # check if parsed string is the same
  expect_equal(modString, parsedModel$modelString)
  expect_equal(length(parsedModel$Variables), 6)
  nVars = length(parsedModel$Variables)
  v = c()
  for(i in seq(1, nVars, 1)){
    nDecl = as.symbol(paste0("decl",i))
    v = c(v, parsedModel$Variables[[nDecl]][["name"]])
  }
  expect_equal(v, c("n", "OBJ", "capacity", "profit", "size", "x"))
  #Constraints
  expect_equal(parsedModel$Constraints$constraint1$varsInvolved, c("OBJ","i", "x"))
  expect_equal(parsedModel$Constraints$constraint2$varsInvolved, c("OBJ", "capacity", "i", "size", "x"))
  #Solve Items
  expect_equal(parsedModel$SolveType$objective, "maximize")
  expect_equal(parsedModel$SolveType$varsInvolved, c( "OBJ", "i", "profit", "x"))
})

test_that("knapsack_1 problem can also be created and solved", {
  library(rminizinc)
  
  # create the variable and parameter declarations
  par_ti = TypeInst$new(Type$new(base_type = "INT", kind = "parameter"))
  par1 = VarDecl$new(id = "n", par_ti, type_inst = par_ti)
  # create the Item 1
  item1 = VarDeclItem$new(decl = par1)
  
  par2_ti = TypeInst$new(Type$new(base_type = "INT", kind = "parameter"))
  par2 = VarDecl$new(type_inst = par2_ti, id = "capacity")
  # create the item 3
  item2 = VarDeclItem$new(decl = par2)
  
  par3_ti = TypeInst$new(Type$new(base_type = "INT", kind = "parameter", dim = 1),
                         indexExprVec = SetVal$new(c(l = 1, u = par1)))
  par3 = VarDecl$new(type_inst = par3_ti, id = "profit")
  # create the item 4
  item3 = VarDeclItem$new(decl = par3)
  
  par4_ti = TypeInst$new(Type$new(base_type = "INT", kind = "parameter", dim = 1),
                         indexExprVec = SetVal$new(c(l = 1, u = par1)))
  par4 = VarDecl$new(type = par4_ti, id = "size")
  # create the item 5
  item4 = VarDeclItem$new(decl = par4)
  
  par5_ti = TypeInst$new(Type$new(base_type = "INT", kind = "decision", dim = 1),
                         indexExprVec = SetVal$new(c(l = 1, u = par1)),
                         domain = SetVal$new(val = c(l=0, u=1)))
  par5 = VarDecl$new(type = par5_ti, id = "x")
  
  #create the item 6
  item5 = VarDeclItem$new(decl = par5)
  
  gen_sum = Generator$new(IN = SetVal$new(c(l = 1, u = par1)), iterator = "i")
  bop2 = Binop$new(lhs_expression = ArrayAccess$new(id = par4$id(), index = gen_sum$iter_id()), binop = "*", 
                   rhs_expression = ArrayAccess$new(id = par5$id() ,index = gen_sum$iter_id()))
  Comp2 = Comprehension$new(generators = list(gen_sum), expression = bop2)
  cl2 = Call$new(fn_id = "sum", lExp = list(Comp2))
  bop3 = Binop$new(lhs_expression = cl2, binop = "<=", rhs_expression = par2$id())
  # item8
  item6 = ConstraintItem$new(expression = bop3)
  
  gen_sum = Generator$new(IN = SetVal$new(c(l = 1, u = par1)), iterator = "i")
  
  bop4 = Binop$new(lhs_expression = ArrayAccess$new(id = par3$id(),index = gen_sum$iter_id()), 
                   binop = "*", rhs_expression = ArrayAccess$new(id = par5$id(), index = gen_sum$iter_id()))
  
  Comp3 = Comprehension$new(generators = list(gen_sum), expression = bop4)
  
  cl3 = Call$new(fn_id = "sum", lExp = list(Comp3))
  
  # item9
  item7 = SolveItem$new(solve_type = "maximize", expression = cl3)
  
  items  = c(item1, item2, item3, item4, item5, item6, item7)
  mod = Model$new(items = items)
  modString = mod$mzn_string()
  
  missingValues = rminizinc::getMissingPars(modelString = modString)
  expect_equal(missingValues, c("n", "capacity", "profit", "size"))
  
  # list of the data
  pVals = list(3,9,c(15,10,7),c(4,3,2))
  names(pVals) = missingValues
  
  # set the missing parameters
  modString = rminizinc:::set_params(modData = pVals,modelString = modString, modify_mzn = FALSE)
  
  # check that no parameters are missing now
  missingValues = rminizinc::getMissingPars(modelString = modString)
  expect_equal(length(missingValues), 0)
  
  # R List object containing the solutions
  solObj = rminizinc:::mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                                libpath = "/snap/minizinc/current/share/minizinc")
})