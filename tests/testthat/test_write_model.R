test_that("check if the variable declarations are correct",{
  
  # only integer value should be supplied to the IntVal classs
  expect_error(IntVal$new(4.3))
  # set values  -- currently only integer ranges are supported
  expect_error(SetVal$new(c(l=1.1, u=2.1)))
  # set can't be assigned an index
  expect_error(TypeInst$new(Type$new(base_type = "int", kind = "par", set_type = TRUE),
                            indexExprVec = Set$new(SetVal$new(c(l=1, u=2)))))
})

test_that("'knapsack problems can be created",{
  # create the variable and parameter declarations
  item1 = VarDeclItem$new(decl = IntDecl(name = "n", kind = "par"))
  
  par2_val = BinOp$new(lhs = Int$new(1), binop = "..", rhs = item1$getDecl()$id())
  item2 = VarDeclItem$new(decl = IntSetDecl(name = "OBJ", kind = "par", value = par2_val))
  
  item3 = VarDeclItem$new(decl = IntDecl(name = "capacity", kind = "par"))
  
  item4 = VarDeclItem$new(decl = IntArrDecl(name = "profit", kind = "par", ndim = 1, 
                                            ind = list(item2$getDecl()$id())))
  
  item5 = VarDeclItem$new(decl = IntArrDecl(name = "size", kind = "par", ndim = 1, ind = list(item2$getDecl()$id())))
  
  item6 = VarDeclItem$new(decl = IntArrDecl(name = "x", kind = "var", ndim = 1, ind = list(item2$getDecl()$id())))
  
  # create the constraints
  
  # declare parameter for iterator
  parIter = IntDecl(name = "i", kind = "par")
  
  gen_forall = Generator$new(IN = item2$getDecl()$id(), decls = list(parIter))
  bop1 = BinOp$new(lhs = ArrayAccess$new(v = item6$getDecl()$id(),  args= list(gen_forall$decl(1))),
                   binop = ">=", rhs = Int$new(0))
  
  Comp1 = Comprehension$new(generators = list(gen_forall), body = bop1, set = FALSE)
  cl1 = Call$new(fnName = "forall", args = list(Comp1))
  item7 = ConstraintItem$new(e = cl1)
  
  gen_sum = Generator$new(IN = item2$getDecl()$id(), decls = list(parIter))
  
  bop2 = BinOp$new(lhs = ArrayAccess$new(v = item5$getDecl()$id(), args = list(gen_sum$decl(1))),                  
                   binop = "*",  rhs = ArrayAccess$new(v = item6$getDecl()$id() , 
                                                       args = list(gen_sum$decl(1))))
  
  Comp2 = Comprehension$new(generators = list(gen_sum), body = bop2, set = FALSE)
  cl2 = Call$new(fnName = "sum", args = list(Comp2))
  bop3 = BinOp$new(lhs = cl2, binop = "<=", rhs = item3$getDecl()$id())
  item8 = ConstraintItem$new(e = bop3)
  
  # create solve type
  
  bop4 = BinOp$new(lhs = ArrayAccess$new(v = item4$getDecl()$id(), args = list(gen_sum$decl(1))),
                   binop = "*", rhs = ArrayAccess$new(v = item6$getDecl()$id(), 
                                                      args = list(gen_sum$decl(1))))
  
  Comp3 = Comprehension$new(generators = list(gen_sum), body = bop4, set = FALSE)
  
  cl3 = Call$new(fnName = "sum", args = list(Comp3))
  
  item9 = SolveItem$new(solve_type = "maximize", e = cl3)
  
  
  # combine to create model
  items  = c(item1, item2, item3, item4, item5, item6, item7, item8, item9)
  mod = Model$new(items = items)
  modString = mod$mzn_string()
  
  pVals= list(3,9,c(15,10,7),c(4,3,2))
  names(pVals) = c(item1$getDecl()$id()$getId(), item3$getDecl()$id()$getId(),
                   item4$getDecl()$id()$getId(), item5$getDecl()$id()$getId())
  
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
