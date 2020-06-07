## -----------------------------------------------------------------------------
library(rminizinc)
# declare an array parameter (only dimensions upto 5 are allowed)
par = variable$new(type = "array", kind = "parameter", sub_type = "int", value = array(1:50, c(10,10,10,10,10)))
# declare a floating point decision variable with a domain of (0,1)
dec = variable$new(type = "float", kind = "decision", domain = c(0, 1))

## -----------------------------------------------------------------------------
par = variable$new(type = "float", kind = "parameter", value = 3)
# decision variables can be declared without a domain also
var1 = variable$new(type = "float", kind = "decision")
var2 = variable$new(type = "float", kind = "decision", domain = c(0, par$value))
# get the LHS expression of the constraint
LHS_expr = get_expression$new(variables = c(var1,var2), arithmetic_operator = "+" )
# get the right hand side expression of the constraint
RHS_expr = get_expression$new(variables = c(var1,var2), arithmetic_operator = "*" )
constr = constraint$new(operator = ">=", LHS_expression = LHS_expr, RHS_expression = RHS_expr)

## -----------------------------------------------------------------------------
obj <- objective$new(type_of_problem = "satisfy")

## -----------------------------------------------------------------------------
# decision variables

# number of bananas
v1 = variable$new(type = "int", kind = "decision", domain = c(0, 100), 
                  name = "b")
# number pf cakes
v2 = variable$new(type = "int", kind = "decision", domain = c(0, 100), 
                  name = "c")


vars = c(v1, v2)

# constraints
c1 = constraint$new(operator = "<=", LHS_expr = get_expression$new(variables = c(250,v1,200,v2),
                                                          arithmetic_operator = c("*","+","*")), 
                                                RHS_expr = get_expression$new(variables = 4000))
c2 = constraint$new(operator = "<=", LHS_expr = get_expression$new(variables = c(2, v1),
                                                          arithmetic_operator = "*"), 
                                                RHS_expr = get_expression$new(variables = 6))
c3 = constraint$new(operator = "<=", LHS_expr = get_expression$new(variables = c(75,v1,150,v2),
                                                          arithmetic_operator = c("*","+","*")), 
                                     RHS_expr = get_expression$new(variables = 2000))
c4 = constraint$new(operator = "<=", LHS_expr = get_expression$new(variables = c(100,v1,150,v2),
                                                          arithmetic_operator = c("*","+","*")), 
                                     RHS_expr = get_expression$new(variables = 500))
c5 = constraint$new(operator = "<=", LHS_expr = get_expression$new(variables = c(75, v2),
                                                          arithmetic_operator = "*"), 
                                                RHS_expr = get_expression$new(variables = 500))


constr = c(c1, c2, c3, c4, c5)

objective_of_problem  = objective$new(type_of_problem = "maximize", 
                                      arithmetic_expression = get_expression$new(variables = c(400,v1,450,v2),
                                                                        arithmetic_operator = c("*","+","*")))

# create the model
m = model$new(decision = vars, constraints = constr, 
              objective = objective_of_problem)

## -----------------------------------------------------------------------------
solution <- results$new(model = m, result_type = "R6", all_solutions = TRUE)
# show the solution
print(solution$result$optimal_solution)

## ----echo=FALSE---------------------------------------------------------------
# remove the temporary model file 
file.remove(solution$mzn)

## ----Workflow 1, echo=FALSE, out.width = '100%'-------------------------------
knitr::include_graphics(paste0(getwd(),"/workflows/first_approach.png"))

## ----Workflow 2, echo=FALSE, out.width = '100%'-------------------------------
knitr::include_graphics(paste0(getwd(),"/workflows/ongoing_approach.png"))

## -----------------------------------------------------------------------------
rminizinc:::mzn_parse("var 0..100: b; var 0..100: c; constraint 250*b + 200*c <= 4000; constraint 2*b <= 6; constraint 75*b + 150*c <= 2000; constraint 100*b + 150*c <= 500; constraint 75*c <= 500; solve maximize 400*b + 450*c;")

## -----------------------------------------------------------------------------
modelString="int: n=3; set of int: OBJ = 1..n; int: capacity=9; array[OBJ] of int: profit=[15,10,7]; array[OBJ] of int: size=[4,3,2]; array[OBJ] of var int: x; constraint forall(i in OBJ)(x[i] >= 0); constraint sum(i in OBJ)(size[i] * x[i]) <= capacity; solve maximize sum(i in OBJ)(profit[i] * x[i]);"
rminizinc:::mzn_eval(modelString = modelString, solver = "org.gecode.gecode",
                     libpath = "/snap/minizinc/current/share/minizinc")

