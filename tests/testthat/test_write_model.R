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
  
  par_ti = TypeInst$new(Type$new(base_type = "int", kind = "parameter"))
  par1 = VarDecl$new(id = "n", type_inst = par_ti)
  item1 = VarDeclItem$new(decl = par1)
  
  par2_val = BinOp$new(lhs_expression = Int$new(IntVal$new(1)), binop = "..", rhs_expression =  par1$id())
  par2_ti = TypeInst$new(Type$new(base_type = "int", kind = "parameter", dim = 1, set_type = TRUE))
  par2 = VarDecl$new(type_inst = par2_ti, e = par2_val, id = "OBJ")
  item2 = VarDeclItem$new(decl = par2)
  
  par3_ti = TypeInst$new(Type$new(base_type = "int", kind = "parameter"))
  par3 = VarDecl$new(type_inst = par3_ti, id = "capacity")
  item3 = VarDeclItem$new(decl = par3)
  
  par4_ti = TypeInst$new(Type$new(base_type = "int", kind = "parameter", dim = 1),
                         indexExprVec = par2$id())
  par4 = VarDecl$new(type_inst = par4_ti, id = "profit")
  item4 = VarDeclItem$new(decl = par4)
  
  par5_ti = TypeInst$new(Type$new(base_type = "int", kind = "parameter", dim = 1),
                         indexExprVec = par2$id())
  par5 = VarDecl$new(type = par5_ti, id = "size")
  item5 = VarDeclItem$new(decl = par5)
  
  par6_ti = TypeInst$new(Type$new(base_type = "int", kind = "decision", dim = 1),
                         indexExprVec = par2$id())
  par6 = VarDecl$new(type = par6_ti, id = "x")
  item6 = VarDeclItem$new(decl = par6)
  
  # create the constraints
  
  # declare parameter for iterator
  par_ti = TypeInst$new(Type$new(base_type = "int", kind = "parameter"))
  parIter = VarDecl$new(id = "i", par_ti, type_inst = par_ti)
  # generator
  gen_forall = Generator$new(IN = par2$id(), decls = list(parIter))
  # binary operator expression
  bop1 = BinOp$new(lhs_expression = ArrayAccess$new(v = par6$id(), 
                                                    args= list(gen_forall$decl(1))),
                   binop = ">=", rhs_expression = Int$new(value = IntVal$new(0)))
  # comprehension
  Comp1 = Comprehension$new(generators = list(gen_forall), e = bop1)
  # forall function call
  cl1 = Call$new(fnName = "forall", args = list(Comp1))
  item7 = ConstraintItem$new(e = cl1)
  
  gen_sum = Generator$new(IN = par2$id(), decls = list(parIter))
  
  bop2 = BinOp$new(lhs_expression = ArrayAccess$new(v = par5$id(), args = list(gen_sum$decl(1))),                  binop = "*",  rhs_expression = ArrayAccess$new(v = par6$id() , 
                                                                                                                                                                  args = list(gen_sum$decl(1))))
  Comp2 = Comprehension$new(generators = list(gen_sum), e = bop2)
  cl2 = Call$new(fnName = "sum", args = list(Comp2))
  bop3 = BinOp$new(lhs_expression = cl2, binop = "<=", rhs_expression = par3$id())
  item8 = ConstraintItem$new(e = bop3)
  
  # create solve type
  
  bop4 = BinOp$new(lhs_expression = ArrayAccess$new(v = par4$id(), args = list(gen_sum$decl(1))),
                   binop = "*", rhs_expression = ArrayAccess$new(v = par6$id(), 
                                                                 args = list(gen_sum$decl(1))))
  
  Comp3 = Comprehension$new(generators = list(gen_sum), e = bop4)
  
  cl3 = Call$new(fnName = "sum", args = list(Comp3))
  
  item9 = SolveItem$new(solve_type = "maximize", e = cl3)
  
  # combine to create model
  
  items  = c(item1, item2, item3, item4, item5, item6, item7, item8, item9)
  mod = Model$new(items = items)
  modString = mod$mzn_string()
  pVals= list(3,9,c(15,10,7),c(4,3,2))
  names(pVals) = c(par1$id()$getId(), par3$id()$getId(), par4$id()$getId(), par5$id()$getId())
  
  modString = rminizinc:::set_params(modData = pVals, modelString = modString)
  
  sol = rminizinc:::mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                             libpath = "/snap/minizinc/current/share/minizinc")
  # there are 7 solutions
  expect_equal(length(sol$SOLUTIONS), 7)
  # check the value of the solutions
  expect_equal(sol$SOLUTIONS$OPTIMAL_SOLUTION$x, c(1,1,1))
  
  # check if parsed model is same 
  parsedModel = rminizinc:::mzn_parse(modelString = modString)
  # check if parsed string is the same
  expect_equal(modString, parsedModel$MODEL_STRING)
  expect_equal(length(parsedModel$VARIABLES), 6)
  nVars = length(parsedModel$VARIABLES)
  v = c()
  for(i in seq(1, nVars, 1)){
    nDecl = as.symbol(paste0("DECL",i))
    v = c(v, parsedModel$VARIABLES[[nDecl]][["DETAILS"]][["NAME"]])
  }
  expect_equal(v, c("n", "OBJ", "capacity", "profit", "size", "x"))
  
  #Solve Items
  expect_equal(parsedModel$SOLVE_TYPE$OBJECTIVE, "maximize")
})
