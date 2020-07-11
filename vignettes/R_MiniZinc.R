## -----------------------------------------------------------------------------
library(rminizinc)

# create the variable and parameter declarations

par1_name = Id$new(ID = "n")
par_dt = Int$new(value = 4)
par_tp = Type$new(base_type = "INT", kind = "parameter")
par1 = VarDecl$new(expression = par_dt, type = par_tp, id = par1_name)
# create the Item 1
item1 = VarDeclItem$new(decl = par1)

par2_name = Id$new(ID = "OBJ")
sv = SetVal$new(val = c(l = 1, u=4))
par2_dt = Set$new(setVal = sv)
par2_tp = Type$new(base_type = "INT", kind = "parameter")
par2 = VarDecl$new(type = par2_tp, expression = par2_dt, id = par2_name)
# create the item 2
item2 = VarDeclItem$new(decl = par2)

par3_name = Id$new(ID = "capacity")
par3_tp = Type$new(base_type = "INT", kind = "parameter")
par3 = VarDecl$new(type = par3_tp, id = par3_name)
# create the item 3
item3 = VarDeclItem$new(decl = par3)

par4_name = Id$new(ID = "profit")
par4_tp = Type$new(base_type = "INT", type_inst = TypeInst$new(Id$new(ID = "OBJ")), kind = "parameter")
par4 = VarDecl$new(type = par4_tp, id = par4_name)
# create the item 4
item4 = VarDeclItem$new(decl = par4)

par5_name = Id$new(ID = "size")
par5_tp = Type$new(base_type = "INT", type_inst = TypeInst$new(indexExprVec = Id$new(ID = "OBJ")), kind = "parameter")
par5 = VarDecl$new(type = par5_tp, id = par5_name)
# create the item 5
item5 = VarDeclItem$new(decl = par5)

par6_name = Id$new(ID = "x")
par6_tp = Type$new(base_type = "INT", kind = "decision", type_inst = TypeInst$new(indexExprVec = Id$new(ID = "OBJ")))
par6 = VarDecl$new(type = par6_tp, id = par6_name)
#create the item 6
item6 = VarDeclItem$new(decl = par6)

## -----------------------------------------------------------------------------
gen_forall = list(Generator$new(IN = Id$new(ID = "OBJ")))
bop1 = Binop$new(lhs_expression = Id$new(ID = "x"),binop = ">=", rhs_expression = Int$new(value = 0))
Comp1 = Comprehension$new(generators = gen_forall, expression = bop1)
# item7 
item7 = ConstraintItem$new(expression = Comp1)

gen_sum = list(Generator$new(IN = Id$new(ID = "OBJ")))
bop2 = Binop$new(lhs_expression = Id$new(ID = "size"), binop = "*", rhs_expression = Id$new(ID = "x"))
Comp2 = Comprehension$new(generators = gen_sum, expression = bop2)
bop3 = Binop$new(lhs_expression = Comp2, binop = "<=", rhs_expression = Id$new(ID = "capacity"))

# item8
item8 = ConstraintItem$new(expression = bop3)

## -----------------------------------------------------------------------------
gen_sum = list(Generator$new(IN = Id$new(ID = "OBJ")))
bop4 = Binop$new(lhs_expression = Id$new(ID = "profit"), binop = "*", rhs_expression = Id$new(ID = "x"))
Comp3 = Comprehension$new(generators = gen_sum, expression = bop4)
# item9
solveType = SolveItem$new(solve_type = "maximize", expression = Comp3)

## ----Workflow 1, echo=FALSE, out.width = '100%'-------------------------------
knitr::include_graphics(paste0(getwd(),"/workflows/first_approach.png"))

## ----Workflow 2, echo=FALSE, out.width = '100%'-------------------------------
knitr::include_graphics(paste0(getwd(),"/workflows/ongoing_approach.png"))

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


