## -----------------------------------------------------------------------------
library(rminizinc)

# create the variable and parameter declarations
item1 = VarDeclItem$new(decl = IntDecl(name = "n", kind = "par"))

par2_val = BinOp$new(lhs = Int$new(1), binop = "..", rhs = item1$getDecl()$id())
item2 = VarDeclItem$new(decl = IntSetDecl(name = "OBJ", kind = "par", value = par2_val))

item3 = VarDeclItem$new(decl = IntDecl(name = "capacity", kind = "par"))

item4 = VarDeclItem$new(decl = IntArrDecl(name = "profit", kind = "par", ndim = 1, 
                                          ind = list(item2$getDecl()$id())))

item5 = VarDeclItem$new(decl = IntArrDecl(name = "size", kind = "par", ndim = 1, ind = list(item2$getDecl()$id())))

item6 = VarDeclItem$new(decl = IntArrDecl(name = "x", kind = "var", ndim = 1, ind = list(item2$getDecl()$id())))

## -----------------------------------------------------------------------------

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

## -----------------------------------------------------------------------------

bop4 = BinOp$new(lhs = ArrayAccess$new(v = item4$getDecl()$id(), args = list(gen_sum$decl(1))),
                      binop = "*", rhs = ArrayAccess$new(v = item6$getDecl()$id(), 
                      args = list(gen_sum$decl(1))))

Comp3 = Comprehension$new(generators = list(gen_sum), body = bop4, set = FALSE)

cl3 = Call$new(fnName = "sum", args = list(Comp3))

item9 = SolveItem$new(solve_type = "maximize", e = cl3)

## -----------------------------------------------------------------------------
items  = c(item1, item2, item3, item4, item5, item6, item7, item8, item9)
mod = Model$new(items = items)
modString = mod$mzn_string()
cat(modString)

## -----------------------------------------------------------------------------
dzn_path = paste0(dirname(getwd()), "/inst/extdata/mzn_examples/knapsack/knapsack_0.dzn")
sol = rminizinc:::mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                      libpath = "/snap/minizinc/current/share/minizinc", dznpath = dzn_path)
print(sol$SOLUTIONS$OPTIMAL_SOLUTION)

## -----------------------------------------------------------------------------
# delete the item 1
item1$delete()
# check that item1 has been deleted
ls(pattern = "item[^a-z]")
# item1 reference is deleted but items vector should be updated
items = c(item2, item3, item4, item5, item6, item7, item8, item9)
mod = Model$new(items)
cat(mod$mzn_string())

## ----Workflow 1, echo=FALSE, out.width = '80%'--------------------------------
knitr::include_graphics(paste0(getwd(),"/workflows/write_model.png"))

## ----Workflow 2, echo=FALSE, out.width = '80%'--------------------------------
knitr::include_graphics(paste0(getwd(),"/workflows/API.png"))

## ----BASIC EXPRESSIONS, echo=FALSE, out.width = '80%'-------------------------
knitr::include_graphics(paste0(getwd(),"/workflows/Basic_Types.png"))

## ----PARENT EXPRESSIONS, echo=FALSE, out.width = '80%'------------------------
knitr::include_graphics(paste0(getwd(),"/workflows/Parent_Types.png"))

## -----------------------------------------------------------------------------
# mzn file path
mzn_path = paste0(dirname(getwd()), "/inst/extdata/mzn_examples/jobshop/jobshop_0.mzn")

# parse the model
parseObj=rminizinc:::mzn_parse(mznpath = mzn_path)

# get the modelString
modString = parseObj$MODEL_STRING

# dzn file path
dzn_path = paste0(dirname(getwd()), "/inst/extdata/mzn_examples/jobshop/jobshop.dzn")

# R List object containing the solutions
solObj = rminizinc:::mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                     libpath = "/snap/minizinc/current/share/minizinc", dznpath = dzn_path)
# get all the solutions
print(solObj$SOLUTIONS)

## -----------------------------------------------------------------------------
# file path
mzn_path = paste0(dirname(getwd()), "/inst/extdata/mzn_examples/knapsack/knapsack_0.mzn")

# get missing parameter values
missingVals=rminizinc:::getMissingPars( mznpath = mzn_path)
print(missingVals)

# list of the data
pVals = list(3, 9, c(15,10,7), c(4,3,2))
names(pVals) = missingVals

# set the missing parameters
modString = rminizinc:::set_params(modData = pVals,mznpath = mzn_path, modify_mzn = FALSE)

# R List object containing the solutions
solObj = rminizinc:::mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                     libpath = "/snap/minizinc/current/share/minizinc")
# get all the solutions
print(solObj$SOLUTIONS)


## -----------------------------------------------------------------------------
# mzn file path
mzn_path = paste0(dirname(getwd()), "/inst/extdata/mzn_examples/production_planning/prod_plan_0.mzn")

# parse the model
parseObj = rminizinc:::mzn_parse(mznpath = mzn_path)

modString = getRModel(parseObj)$mzn_string()

# dzn file path
dzn_path = paste0(dirname(getwd()), "/inst/extdata/mzn_examples/production_planning/prod_plan_0.dzn")

# R List object containing the solutions
solObj = rminizinc:::mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                     libpath = "/snap/minizinc/current/share/minizinc", dznpath = dzn_path)
# get all the solutions
print(solObj$SOLUTIONS)

