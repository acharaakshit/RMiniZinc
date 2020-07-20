## ----VarDecl UML, echo=FALSE, out.width = '30%'-------------------------------
knitr::include_graphics(paste0(getwd(),"/workflows/VarDecl.png"))

## ----Expression UML, echo=FALSE, out.width = '80%'----------------------------
knitr::include_graphics(paste0(getwd(),"/workflows/ExpressionUml.png"))

## -----------------------------------------------------------------------------
library(rminizinc)

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

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------

gen_sum = Generator$new(IN = par2$id(), iterator = "i")

bop4 = Binop$new(lhs_expression = ArrayAccess$new(id = par4$id(),index = gen_sum$iter_id()), 
                      binop = "*", rhs_expression = ArrayAccess$new(id = par6$id(), index = gen_sum$iter_id()))

Comp3 = Comprehension$new(generators = list(gen_sum), expression = bop4)

cl3 = Call$new(fn_id = "sum", lExp = list(Comp3))

# item9
item9 = SolveItem$new(solve_type = "maximize", expression = cl3)

## ----Model UML, echo=FALSE, out.width = '70%'---------------------------------
knitr::include_graphics(paste0(getwd(),"/workflows/ItemUml.png"))

## -----------------------------------------------------------------------------
items  = c(item1, item2, item3, item4, item5, item6, item7, item8, item9)
mod = Model$new(items = items)
modString = mod$mzn_string()

## -----------------------------------------------------------------------------
dzn_path = paste0(dirname(getwd()), "/mzn_examples/knapsack/knapsack_0.dzn")
sol = rminizinc:::mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                      libpath = "/snap/minizinc/current/share/minizinc", dznpath = dzn_path)

## ----Workflow 1, echo=FALSE, out.width = '100%'-------------------------------
knitr::include_graphics(paste0(getwd(),"/workflows/write_model.png"))

## ----Workflow 2, echo=FALSE, out.width = '100%'-------------------------------
knitr::include_graphics(paste0(getwd(),"/workflows/API.png"))

## -----------------------------------------------------------------------------
# mzn file path
mzn_path = paste0(dirname(getwd()), "/mzn_examples/knapsack/knapsack_0.mzn")

# parse the model
parseObj=rminizinc:::mzn_parse(mznpath = mzn_path)

# get the modelString
modString = parseObj$modelString

# dzn file path
dzn_path = paste0(dirname(getwd()), "/mzn_examples/knapsack/knapsack_0.dzn")

# R List object containing the solutions
solObj = rminizinc:::mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                     libpath = "/snap/minizinc/current/share/minizinc", dznpath = dzn_path)
# get all the solutions
print(solObj$Solutions)

## -----------------------------------------------------------------------------
# file path
mzn_path = paste0(dirname(getwd()), "/mzn_examples/knapsack/knapsack_0.mzn")

# get missing parameter values
missingVals=rminizinc:::getMissingPars( mznpath = mzn_path)
print(missingVals)

# list of the data
pVals = list(3,9,c(15,10,7),c(4,3,2))
names(pVals) = missingVals

# set the missing parameters
modString = rminizinc:::set_params(modData = pVals,mznpath = mzn_path, modify_mzn = FALSE)

# R List object containing the solutions
solObj = rminizinc:::mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                     libpath = "/snap/minizinc/current/share/minizinc")
# get all the solutions
print(solObj$Solutions)


## -----------------------------------------------------------------------------
# file path
mzn_path = paste0(dirname(getwd()), "/mzn_examples/knapsack/knapsack_0.mzn")

# parse the model
parseObj=rminizinc:::mzn_parse(mznpath = mzn_path)

print("The original model:")
cat(parseObj$modelString)

# delete the first variable declaration
item_number  = parseObj$Variables$decl1$itemNo

# delete the item but don't update the mzn
print("The updated model string is:")
cat(deleteItem(itemNo = item_number, mznpath = mzn_path, updateMZN = FALSE))


