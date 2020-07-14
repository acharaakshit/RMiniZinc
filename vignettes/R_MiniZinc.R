## -----------------------------------------------------------------------------
library(rminizinc)

# create the variable and parameter declarations

par_dt = Int$new(value = 4)
par_tp = Type$new(base_type = "INT", kind = "parameter")
par1 = VarDecl$new(expression = par_dt, type = par_tp, id = "n")
# create the Item 1
item1 = VarDeclItem$new(decl = par1)

sv = SetVal$new(val = c(l = 1, u=3))
par2_dt = Set$new(setVal = sv)
par2_tp = Type$new(base_type = "INT", kind = "parameter", dim = 1, set_type = TRUE)
par2 = VarDecl$new(type = par2_tp, expression = par2_dt, id = "OBJ")
# create the item 2
item2 = VarDeclItem$new(decl = par2)

par3_tp = Type$new(base_type = "INT", kind = "parameter")
par3 = VarDecl$new(type = par3_tp, id = "capacity")
# create the item 3
item3 = VarDeclItem$new(decl = par3)

par4_tp = Type$new(base_type = "INT", type_inst = TypeInst$new(par2$id()), kind = "parameter", dim = 1)
par4 = VarDecl$new(type = par4_tp, id = "profit")
# create the item 4
item4 = VarDeclItem$new(decl = par4)

par5_tp = Type$new(base_type = "INT", type_inst = TypeInst$new(par2$id()), kind = "parameter", dim = 1)
par5 = VarDecl$new(type = par5_tp, id = "size")
# create the item 5
item5 = VarDeclItem$new(decl = par5)

par6_tp = Type$new(base_type = "INT", kind = "decision", type_inst = TypeInst$new(par2$id()), dim = 1)
par6 = VarDecl$new(type = par6_tp, id = "x")
#create the item 6
item6 = VarDeclItem$new(decl = par6)

## -----------------------------------------------------------------------------
gen_forall = Generator$new(IN = par2$id(), iterator = "i")
bop1 = Binop$new(lhs_expression = ArrayAccess$new(id = par6$id(), index = gen_forall$iter_id()), binop = ">=", 
                                        rhs_expression =    Int$new(value = 0))
Comp1 = Comprehension$new(generators = list(gen_forall), expression = bop1)
cl1 = Call$new(fn_id = "forall", lExp = list(Comp1))
# item7 
item7 = ConstraintItem$new(expression = cl1)
item7$c_str()

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


