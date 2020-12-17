## -----------------------------------------------------------------------------
library(rminizinc)
# load the project directory
data("proot")
# check if the library is present
data("config")
parse.next = FALSE
if(LIBMINIZINC_PATH == ""){
  warning("Please install libminizinc on your system!")
  parse.next = TRUE
}
# check if solver binaries are present
data("slvbin")
evaluate.next = FALSE
if(SOLVER_BIN == ""){
  warning("Please download solver binaries to solve the model")
  evaluate.next = TRUE
}

# check if vignettes should be executed
if (!requireNamespace("rmarkdown") ||
!rmarkdown::pandoc_available("1.14")) {
     warning(call. = FALSE, "This vignette assumes that rmarkdown and pandoc
version 1.14 are available. These were not found. Older versions will not work.")
     knitr::knit_exit()
   }

## ---- error=parse.next--------------------------------------------------------
# mzn file path
mzn_path = paste0(PROJECT_DIRECTORY, "/inst/extdata/mzn_examples/jobshop/jobshop_0.mzn")

# parse the model
parseObj = rminizinc:::mzn_parse(mzn_path = mzn_path)

## ---- error=parse.next--------------------------------------------------------
missingPars = get_missing_pars(model = parseObj)
print(missingPars)

## ---- error=parse.next--------------------------------------------------------
pVals = list(Int$new(3), Int$new(4),
             Array$new(exprVec = intExpressions(c(3, 3, 4, 4, 4, 3, 2, 2, 3, 3, 3, 4)),
               dimranges = list(IntSetVal$new(1,3), IntSetVal$new(1,4))), 
             Array$new(exprVec = intExpressions(c(1, 2, 3, 4, 1, 3, 2, 4, 4, 2, 1, 3)),
               dimranges = list(IntSetVal$new(1,3), IntSetVal$new(1,4))))
names(pVals) = missingPars
model = set_params(model = parseObj, modData = pVals)
cat(model$mzn_string())

## ---- error=evaluate.next-----------------------------------------------------
# R List object containing the solutions
solObj = rminizinc:::mzn_eval(model, solver = "org.gecode.gecode",
                   lib_path = paste0(PROJECT_DIRECTORY, "/inst/minizinc/"))
# get all the solutions
print(solObj$SOLUTIONS)

## ---- error=parse.next--------------------------------------------------------
# file path
mzn_path = paste0(PROJECT_DIRECTORY, "/inst/extdata/mzn_examples/knapsack/knapsack_0.mzn")

# get missing parameter values
missingVals=rminizinc:::get_missing_pars( model = mzn_parse(mzn_path = mzn_path))
print(missingVals)

# list of the data
pVals = list(Int$new(3), Int$new(9), Array$new(intExpressions(c(15,10,7)))
             , Array$new(intExpressions(c(4,3,2))))
names(pVals) = missingVals

# set the missing parameters
model = rminizinc:::set_params(modData = pVals, 
                                   mzn_parse(mzn_path = mzn_path))

## ---- error=evaluate.next-----------------------------------------------------
# R List object containing the solutions
solObj = rminizinc:::mzn_eval(r_model = model)
# get all the solutions
print(solObj$SOLUTIONS)

## ---- error=parse.next--------------------------------------------------------
# create the variable and parameter declarations
decl = IntDecl(name = "n", kind = "par")
item1 = VarDeclItem$new(decl = decl)

par2_val = BinOp$new(lhs = Int$new(1), binop = "..", rhs = item1$getId())
item2 = VarDeclItem$new(decl = IntSetDecl(name = "OBJ", kind = "par", value = par2_val))

item3 = VarDeclItem$new(decl = IntDecl(name = "capacity", kind = "par"))

item4 = VarDeclItem$new(decl = IntArrDecl(name = "profit", kind = "par", ndim = 1, 
                                          ind = list(item2$getId())))

item5 = VarDeclItem$new(decl = IntArrDecl(name = "size", kind = "par", ndim = 1, ind =                                                            list(item2$getId())))

item6 = VarDeclItem$new(decl = IntArrDecl(name = "x", kind = "var", ndim = 1, ind = list(item2$getId())))

## ---- error=parse.next--------------------------------------------------------

# declare parameter for iterator
parIter = IntDecl(name = "i", kind = "par")


gen_forall = Generator$new(IN = item2$getId(), decls = list(parIter))
bop1 = BinOp$new(lhs = ArrayAccess$new(v = item6$getId(),  args= list(gen_forall$getDecl(1)$getId())),
                                                             binop = ">=", rhs = Int$new(0))

Comp1 = Comprehension$new(generators = list(gen_forall), body = bop1, set = FALSE)
cl1 = Call$new(fnName = "forall", args = list(Comp1))
item7 = ConstraintItem$new(e = cl1)

gen_sum = Generator$new(IN = item2$getId(), decls = list(parIter))

bop2 = BinOp$new(lhs = ArrayAccess$new(v = item5$getId(), args = list(gen_sum$getDecl(1)$getId())),             
                 binop = "*",  rhs = ArrayAccess$new(v = item6$getId() , 
                 args = list(gen_sum$getDecl(1)$getId())))

Comp2 = Comprehension$new(generators = list(gen_sum), body = bop2, set = FALSE)
cl2 = Call$new(fnName = "sum", args = list(Comp2))
bop3 = BinOp$new(lhs = cl2, binop = "<=", rhs = item3$getId())
item8 = ConstraintItem$new(e = bop3)

## ---- error=parse.next--------------------------------------------------------

bop4 = BinOp$new(lhs = ArrayAccess$new(v = item4$getId(), args = list(gen_sum$getDecl(1)$getId())),
                      binop = "*", rhs = ArrayAccess$new(v = item6$getId(), 
                      args = list(gen_sum$getDecl(1)$getId())))

Comp3 = Comprehension$new(generators = list(gen_sum), body = bop4, set = FALSE)

cl3 = Call$new(fnName = "sum", args = list(Comp3))

item9 = SolveItem$new(solve_type = "maximize", e = cl3)

## ---- error=parse.next--------------------------------------------------------
items  = c(item1, item2, item3, item4, item5, item6, item7, item8, item9)
mod = Model$new(items = items)
modString = mod$mzn_string()
cat(modString)

## ---- error=parse.next--------------------------------------------------------
# delete the item 1 i.e declaration of n 
item1$delete()
cat(mod$mzn_string())

## ---- results = 'hold', error=parse.next--------------------------------------
declItem = VarDeclItem$new(mzn_str = "set of int: WORKSHEET = 0..worksheets-1;")
sprintf("Is this a parameter? %s", declItem$getDecl()$isPar())
sprintf("Is this a set? %s", declItem$getDecl()$ti()$type()$isSet())
sprintf("Base type of set: %s", declItem$getDecl()$ti()$type()$bt())
sprintf("Name: %s", declItem$getId()$getName())
sprintf("Value: %s", declItem$getDecl()$getValue()$c_str())

## ---- results = 'hold', error=parse.next--------------------------------------
CstrItem = ConstraintItem$new(mzn_str = "constraint forall (i in PREC)
                  (let { WORKSHEET: w1 = preceeds[i];
		                     WORKSHEET: w2 = succeeds[i]; } in
                   g[w1] * e[w1] <= d[w2] + days * (1 - g[w2]));")
sprintf("Expression involved: %s", CstrItem$getExp()$c_str())
sprintf("Call function name: %s", CstrItem$getExp()$getName())
sprintf("Number of Arguments: %s", CstrItem$getExp()$nargs())
sprintf("Class of Argument: %s",  class(CstrItem$getExp()$getArg(1))[1])
sprintf("Number of Generators: %s", CstrItem$getExp()$nargs())
sprintf("Generator: %s", CstrItem$getExp()$getArg(1)$getGen(1)$c_str())
sprintf("Comprehension body: %s", CstrItem$getExp()$getArg(1)$getBody()$c_str())

## ---- results = 'hold', error=parse.next--------------------------------------
SlvItem = SolveItem$new(mzn_str = "solve 
    :: int_search(
        [ if j = 1 then g[import_first[i]] else -d[import_first[i]] endif  | i in 1..worksheets, j in 1..2], 
        input_order, indomain_max, complete)
    maximize objective;")
sprintf("Objective: %s", SlvItem$getSt())
cat(sprintf("Annotation: %s", SlvItem$getAnn()$c_str()))

## ---- results = 'hold', error=parse.next--------------------------------------
fnItem = FunctionItem$new(mzn_str = "predicate nonoverlap(var int:s1, var int:d1,
                     var int:s2, var int:d2)=
          s1 + d1 <= s2 \\/ s2 + d2 <= s1;")
sprintf("Function name: %s", fnItem$name())
sprintf("No of function declarations: %s", length(fnItem$getDecls()))
sprintf("Function expression: %s", fnItem$getBody()$c_str())

## ---- error=parse.next--------------------------------------------------------
iItem = IncludeItem$new(mzn_str = "include \"cumulative.mzn\" ;")
sprintf("Included mzn name: %s", iItem$getmznName())

## ---- results = 'hold', error=parse.next--------------------------------------
vd = VarDomainDecl(name = "n", dom = Set$new(IntSetVal$new(imin = 1, imax = 2)))
sprintf("The current declaration is: %s", vd$c_str())
vd$setDomain(Set$new(IntSetVal$new(imin = 3, imax = 5)))
sprintf("The modified declaration is: %s", vd$c_str())

## ---- results = 'hold', error=parse.next--------------------------------------
vItem = VarDeclItem$new(mzn_str = "set of int: a = {1, 2, 3, 4};") 
cItem = ConstraintItem$new(mzn_str = "constraint sum(a) < 10;")
sprintf("The current constraint is: %s", cItem$c_str())
cItem$setExp(BinOp$new(lhs = Call$new(fnName = "max", args = list(vItem$getDecl()$getId())),
                       binop = "<", rhs = Int$new(10)))
sprintf("The modified constraint is: %s", cItem$c_str())

