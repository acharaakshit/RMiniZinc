## -----------------------------------------------------------------------------
library(rminizinc)

# create the variable and parameter declarations
item1 = VarDeclItem$new(decl = IntDecl(name = "n", kind = "par"))

par2_val = BinOp$new(lhs = Int$new(1), binop = "..", rhs = item1$id())
item2 = VarDeclItem$new(decl = IntSetDecl(name = "OBJ", kind = "par", value = par2_val))

item3 = VarDeclItem$new(decl = IntDecl(name = "capacity", kind = "par"))

item4 = VarDeclItem$new(decl = IntArrDecl(name = "profit", kind = "par", ndim = 1, 
                                          ind = list(item2$id())))

item5 = VarDeclItem$new(decl = IntArrDecl(name = "size", kind = "par", ndim = 1, ind =                                                            list(item2$id())))

item6 = VarDeclItem$new(decl = IntArrDecl(name = "x", kind = "var", ndim = 1, ind = list(item2$id())))

## -----------------------------------------------------------------------------

# declare parameter for iterator
parIter = IntDecl(name = "i", kind = "par")


gen_forall = Generator$new(IN = item2$id(), decls = list(parIter))
bop1 = BinOp$new(lhs = ArrayAccess$new(v = item6$id(),  args= list(gen_forall$decl(1))),
                                                             binop = ">=", rhs = Int$new(0))

Comp1 = Comprehension$new(generators = list(gen_forall), body = bop1, set = FALSE)
cl1 = Call$new(fnName = "forall", args = list(Comp1))
item7 = ConstraintItem$new(e = cl1)

gen_sum = Generator$new(IN = item2$id(), decls = list(parIter))

bop2 = BinOp$new(lhs = ArrayAccess$new(v = item5$id(), args = list(gen_sum$decl(1))),                  
                 binop = "*",  rhs = ArrayAccess$new(v = item6$id() , 
                 args = list(gen_sum$decl(1))))

Comp2 = Comprehension$new(generators = list(gen_sum), body = bop2, set = FALSE)
cl2 = Call$new(fnName = "sum", args = list(Comp2))
bop3 = BinOp$new(lhs = cl2, binop = "<=", rhs = item3$id())
item8 = ConstraintItem$new(e = bop3)

## -----------------------------------------------------------------------------

bop4 = BinOp$new(lhs = ArrayAccess$new(v = item4$id(), args = list(gen_sum$decl(1))),
                      binop = "*", rhs = ArrayAccess$new(v = item6$id(), 
                      args = list(gen_sum$decl(1))))

Comp3 = Comprehension$new(generators = list(gen_sum), body = bop4, set = FALSE)

cl3 = Call$new(fnName = "sum", args = list(Comp3))

item9 = SolveItem$new(solve_type = "maximize", e = cl3)

## -----------------------------------------------------------------------------
items  = c(item1, item2, item3, item4, item5, item6, item7, item8, item9)
mod = Model$new(items = items)
modString = mod$mzn_string()
cat(modString)

## ---- results = 'hold'--------------------------------------------------------
declItem = VarDeclItem$new(mzn_str = "set of int: WORKSHEET = 0..worksheets-1;")
sprintf("Is this a parameter? %s", declItem$getDecl()$isPar())
sprintf("Is this a set? %s", declItem$getDecl()$ti()$type()$isSet())
sprintf("Base type of set: %s", declItem$getDecl()$ti()$type()$bt())
sprintf("Name: %s", declItem$id()$getId())
sprintf("Value: %s", declItem$getDecl()$value()$c_str())

## ---- results = 'hold'--------------------------------------------------------
CstrItem = ConstraintItem$new(mzn_str = "constraint forall (i in PREC)
                  (let { WORKSHEET: w1 = preceeds[i];
		                     WORKSHEET: w2 = succeeds[i]; } in
                   g[w1] * e[w1] <= d[w2] + days * (1 - g[w2]));")
sprintf("Expression involved: %s", CstrItem$getExp()$c_str())
sprintf("Call function name: %s", CstrItem$getExp()$getName())
sprintf("Number of Arguments: %s", CstrItem$getExp()$nargs())
sprintf("Class of Argument: %s",  class(CstrItem$getExp()$getArg(1))[1])
sprintf("Number of Generators: %s", CstrItem$getExp()$nargs())
sprintf("Generator: %s", CstrItem$getExp()$getArg(1)$gen_i(1)$c_str())
sprintf("Comprehension body: %s", CstrItem$getExp()$getArg(1)$getBody()$c_str())

## ---- results = 'hold'--------------------------------------------------------
SlvItem = SolveItem$new(mzn_str = "solve 
    :: int_search(
        [ if j = 1 then g[import_first[i]] else -d[import_first[i]] endif  | i in 1..worksheets, j in 1..2], 
        input_order, indomain_max, complete)
    maximize objective;")
sprintf("Objective: %s", SlvItem$getSt())
cat(sprintf("Annotation: %s", SlvItem$getAnn()$c_str()))

## -----------------------------------------------------------------------------
aDeclItem = VarDeclItem$new(mzn_str = "int: n;")
aItem = AssignItem$new(decl = aDeclItem$getDecl(), mzn_str = "n = 8;")
sprintf("Value: %s", aItem$getValue()$c_str())

## ---- results = 'hold'--------------------------------------------------------
fnItem = FunctionItem$new(mzn_str = "predicate nonoverlap(var int:s1, var int:d1,
                     var int:s2, var int:d2)=
          s1 + d1 <= s2 \\/ s2 + d2 <= s1;")
sprintf("Function name: %s", fnItem$name())
sprintf("No of function declarations: %s", length(fnItem$decls()))
sprintf("Function expression: %s", fnItem$body()$c_str())

## -----------------------------------------------------------------------------
iItem = IncludeItem$new(mzn_str = "include \"cumulative.mzn\" ;")
sprintf("Included mzn name: %s", iItem$getmznName())

## -----------------------------------------------------------------------------
# delete the item 1
item1$delete()
# check that item1 has been deleted
ls(pattern = "item[^a-z]")
# item1 reference is deleted but items vector should be updated
items = c(item2, item3, item4, item5, item6, item7, item8, item9)
mod = Model$new(items)
cat(mod$mzn_string())

## ----BASIC EXPRESSIONS, echo=FALSE, out.width = '80%'-------------------------
knitr::include_graphics(paste0(getwd(),"/workflows/Basic_Types.png"))

## ----PARENT EXPRESSIONS, echo=FALSE, out.width = '80%'------------------------
knitr::include_graphics(paste0(getwd(),"/workflows/Parent_Types.png"))

## -----------------------------------------------------------------------------
# mzn file path
mzn_path = paste0(dirname(getwd()), "/inst/extdata/mzn_examples/jobshop/jobshop_0.mzn")

# parse the model
parseObj=rminizinc:::mzn_parse(mznpath = mzn_path)

## -----------------------------------------------------------------------------
missingPars = get_missing_pars(modelString = parseObj$MODEL_STRING)
print(missingPars)

## -----------------------------------------------------------------------------
pVals = list(3, 4, c(3, 3, 4, 4, 4, 3, 2, 2, 3, 3, 3, 4), 
             c(1, 2, 3, 4, 1, 3, 2, 4, 4, 2, 1, 3))
names(pVals) = missingPars
modString = set_params(modData = pVals, mznpath = mzn_path)

## -----------------------------------------------------------------------------
# R List object containing the solutions
solObj = rminizinc:::mzn_eval(modelString = modString, solver = "org.gecode.gecode",
                     libpath = "/snap/minizinc/current/share/minizinc")
# get all the solutions
print(solObj$SOLUTIONS)

## -----------------------------------------------------------------------------
# file path
mzn_path = paste0(dirname(getwd()), "/inst/extdata/mzn_examples/knapsack/knapsack_0.mzn")

# get missing parameter values
missingVals=rminizinc:::get_missing_pars( mznpath = mzn_path)
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

## ---- results = 'hold'--------------------------------------------------------
vd = VarDomainDecl(name = "n", dom = Set$new(IntSetVal$new(imin = 1, imax = 2)))
sprintf("The current declaration is: %s", vd$c_str())
vd$setDomain(Set$new(IntSetVal$new(imin = 3, imax = 5)))
sprintf("The modified declaration is: %s", vd$c_str())

## ---- results = 'hold'--------------------------------------------------------
vItem = VarDeclItem$new(mzn_str = "set of int: a = {1, 2, 3, 4};") 
cItem = ConstraintItem$new(mzn_str = "constraint sum(a) < 10;")
sprintf("The current constraint is: %s", cItem$c_str())
cItem$setExp(BinOp$new(lhs = Call$new(fnName = "max", args = list(vItem$getDecl()$id())),
                       binop = "<", rhs = Int$new(10)))
sprintf("The modified constraint is: %s", cItem$c_str())

