## ---- results='hold'----------------------------------------------------------
library(rminizinc)

# load the PROJECT_DIRECTORY
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

## ---- error=evaluate.next, results='hold'-------------------------------------
# knapsack problem
result = knapsack(n = 3, capacity = 9, profit = c(15,10,7), size = c(4,3,2))
cat(sprintf("The minizinc representation of the problem is:\n%s", result$model$mzn_string()))
cat(sprintf("The solutions returned by minizinc are:\n%s", result$solution$SOLUTION_STRING))
# R representation of solutions
print(result$solution$SOLUTIONS)

## ---- error=evaluate.next, results='hold'-------------------------------------
# assignment problem
result  = assignment(n = 4, m = 5, cost = c(7,1,3,4,6,8,2,5,1,4,4,3,7,2,5,3,1,6,3,6))
cat(sprintf("The minizinc representation of the problem is:\n%s", result$model$mzn_string()))
cat(sprintf("The solutions returned by minizinc are:\n%s", result$solution$SOLUTION_STRING))
# R representation of solutions
print(result$solution$SOLUTIONS)

## ---- error=parse.next, results='hold'----------------------------------------
# mzn file path

# load the PROJECT_DIRECTORY
data("proot")

"
NOTE: PROJECT_DIRECTORY is the project root directory path. This path is used for pointing to the mzn files provided with the package. If you have installed the package, PROJECT_DIRECTORY will initially point to tmp as the R package installation starts there. However, the next chunk of code will detect that and point to the installed rminizinc root folder. If you have installed rminizinc in some other location please make the PROJECT_DIRECTORY point to that folder.
"

mzn_local_location = "/inst/extdata/mzn_examples/"

if(grepl( "tmp", PROJECT_DIRECTORY, fixed = TRUE)){
  message("PROJECT_DIRECTORY is not a correct location anymore. Now PROJECT_DIRECTORY
          is pointing to the location of installed rminizinc package. Please note that
          the PROJECT_DIRECTORY stored in proot.RData is still the ")
  PROJECT_DIRECTORY = paste0(.libPaths()[1], "/rminizinc")
  mzn_local_location = "/extdata/mzn_examples/"
}

mzn_path = paste0(PROJECT_DIRECTORY, mzn_local_location, "knapsack/knapsack_0.mzn")

# returns the R equivalent of a MiniZinc model
parsedModel = rminizinc:::mzn_parse(mzn_path = mzn_path)

cat(sprintf("The current model is:\n%s", parsedModel$mzn_string()))

## ---- error=parse.next, results='hold'----------------------------------------
missingPars = get_missing_pars(model = parsedModel)
print(missingPars)

## ---- error=parse.next, results='hold'----------------------------------------
# Int$new() for creating a new integer in model
# Array$new() for creating an array
# intExpressions() to create a sequence of integers (useful for array)
pVals = list(Int$new(3), Int$new(9), Array$new(c(Int$new(15), Int$new(10), Int$new(7)),
                                               dimranges = c(IntSetVal$new(1, 3))),
            Array$new(intExpressions(c(4,3,2)), dimranges = c(IntSetVal$new(1, 3))))

names(pVals) = missingPars

model = set_params(model = parsedModel, modData = pVals)

# check ?Model for more details
cat(sprintf("The updated model is:\n %s", model$mzn_string()))

## ---- error=evaluate.next-----------------------------------------------------
# R List object containing the solutions
solObj = rminizinc:::mzn_eval(r_model = model)

# get all the solutions
print(solObj$SOLUTIONS)

## ---- error=parse.next--------------------------------------------------------
# int: a;
a = VarDeclItem$new(decl = IntDecl(name = "a", kind = "par"))
a$c_str()

# var int: a;
a = VarDeclItem$new(decl = IntDecl(name = "a", kind = "var"))
a$c_str()

# 0..3: a;
a = VarDeclItem$new(decl = IntDecl(name = "a", kind = "par", 
                                   domain = Set$new(IntSetVal$new(imin = 0, imax = 3))))
a$c_str()

# set of int: b = 1..5;
b = VarDeclItem$new(decl = IntSetDecl(name = "b", kind = "par", 
                                      value = Set$new(IntSetVal$new(imin = 1, imax = 5))))
b$c_str()

# set of int: b = {1, 3, 5, 7, 9};
b = VarDeclItem$new(decl = IntSetDecl(name = "b", kind = "par", 
                                      value = Set$new(val = intExpressions(c(1, 3, 5 ,7 ,9)))))
b$c_str()

# int: IND = 3;
a = VarDeclItem$new(decl = IntDecl(name = "IND", kind = "par", value = 3))
a$c_str()

val = a$getDecl()$getValue()$getIntVal()
sprintf("The value of a is: %s", val)

# array[IND] of int: p = [15,10,7];
p = VarDeclItem$new(decl = IntArrDecl(name = "p", kind = "par", ind = c(a$getId()),
                                  value = Array$new(exprVec = intExpressions(c(15, 10, 7)),
                                                    dimranges = c(IntSetVal$new(1, val))),
                                        ndim = 1))
p$c_str()

# array[IND] of int: p = [|1, 2, 3 | 4, 5, 6 | 7, 8, 9|];
# Array can only be used to provide 1D and 2D array values
array_value = Array$new(exprVec = c(intExpressions(c(1,2,3)), intExpressions(c(4,5,6)),                                                                    intExpressions(c(7,8,9))),
                                        dimranges = c(IntSetVal$new(1, val), IntSetVal$new(1, val)))
p = VarDeclItem$new(decl = IntArrDecl(name = "p", kind = "par", ind = c(a$getId(), a$getId()),
                    value = array_value, ndim = 2))
cat(p$c_str())

## Recommended way to provide array values
array_value = Call$new(fnName = "array2d", args = c(a$getId(), a$getId(),
                                                    intExpressions(c(1,2,3)), intExpressions(c(4,5,6)),                                                                    intExpressions(c(7,8,9))))
p$getDecl()$setValue(array_value)
cat(p$c_str())

## ---- error=parse.next--------------------------------------------------------
# var int a; constraint a < 10;

# var int a;
a = VarDeclItem$new(decl = IntDecl(name = "a", kind = "var"))
a$c_str()

# a < 10;
exp = BinOp$new(lhs = a$getId(), binop = "<", rhs = Int$new(10))
exp$c_str()

# constraint a < 10;
cnst = ConstraintItem$new(e = exp)
cnst$c_str()

## ---- error=parse.next--------------------------------------------------------
# solve satisfy;
s = SolveItem$new(solve_type = "satisfy")
s$c_str()

# var int: sum; 
sum = IntDecl(name = "sum", kind = "var")
sum$c_str()

# minimize sum;
s = SolveItem$new(solve_type = "minimize", e = sum$getId())
s$c_str()

# maximize sum;
s = SolveItem$new(solve_type = "maximize", e = sum$getId())
s$c_str()

## -----------------------------------------------------------------------------
# always provide ind (array index) in c() or list()
a = VarDeclItem$new(decl = IntArrDecl(name = "a", kind = "var",ind = c(Int$new(4)),ndim = 1))
a$c_str()

iter = IntDecl(name = "i", kind = "par")
generator = Generator$new(IN = a$getId(), decls = list(iter))
bop = BinOp$new(lhs = ArrayAccess$new(v = a$getId(),  args= list(generator$getDecl(1)$getId())),
                                                             binop = ">=", rhs = Int$new(0))

comprehension = Comprehension$new(generators = list(generator), body = bop, set = FALSE)
comprehension$c_str()

## -----------------------------------------------------------------------------
cl = Call$new(fnName = "forall", args = list(comprehension))
cl$c_str()

## -----------------------------------------------------------------------------
# if x < 0 then -1 elseif x > 0 then 1 else 0 endif

# var int:x;
x = IntDecl(name = "x", kind = "var")
x$c_str()

bop1 = BinOp$new(lhs = x$getId(), binop = "<", rhs = Int$new(0))
bop1$c_str()
uop = UnOp$new(args = c(Int$new(1)), op = "-")
uop$c_str()
bop2 = BinOp$new(lhs = x$getId(), binop = "<", rhs = Int$new(0))
bop2$c_str()
exp = Ite$new(ifs = c(bop1, bop2), thens = c(uop, Int$new(1)), Else = Int$new(0))
exp$c_str()

## -----------------------------------------------------------------------------
# let { int: x = 3; int: y = 4; } in x + y
x = VarDeclItem$new(IntDecl(name = "x", kind = "par", value = 3))
y = VarDeclItem$new(IntDecl(name = "y", kind = "par", value = 4))
bop = BinOp$new(lhs = x$getId(), binop = "+", rhs = y$getId())
let = Let$new(let = c(x,y), body = bop)
cat(let$c_str())

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

## -----------------------------------------------------------------------------
a = IntDecl(name = "a", kind = "par", value = 4)
c = IntDecl(name = "c", kind = "par", value = 5)
b = IntDecl(name = "b", kind = "var")

# declaration items
a_item = VarDeclItem$new(decl = a)
b_item = VarDeclItem$new(decl = b)
c_item = VarDeclItem$new(decl = c)

# b > 0 is a binary operation
b_0 = BinOp$new(lhs = b$getId(), binop = ">", rhs = Int$new(0))
constraint1  = ConstraintItem$new(e = b_0)

# a ^ 2 is a binary operation
# a$getId() gives the variable identifier
a_2 = BinOp$new(lhs = a$getId(), binop = "^", Int$new(2))
b_2 = BinOp$new(lhs = b$getId(), binop = "^", Int$new(2))
a2_b2 = BinOp$new(lhs = a_2, binop = "+", rhs = b_2)
c_2 = BinOp$new(lhs = c$getId(), binop = "^", Int$new(2))
a2_b2_c2 = BinOp$new(lhs = a2_b2, binop = "=", rhs = c_2)
constraint2  = ConstraintItem$new(e = a2_b2_c2)

solve  = SolveItem$new(solve_type = "satisfy")

model = Model$new(items = c(a_item, b_item, c_item, constraint1, constraint2, solve))

cat(model$mzn_string())

## ---- error=parse.next--------------------------------------------------------
# delete the item a i.e declaration of a 
a$delete()
cat(model$mzn_string())

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

