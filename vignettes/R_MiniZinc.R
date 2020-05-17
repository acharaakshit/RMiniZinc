## -----------------------------------------------------------------------------
library(rminizinc)
par = variable$new(type = "array", kind = "parameter", value = array(data = c(1,2,3,4,5)))
dec = variable$new(type = "float", kind = "decision", domain = c(0, 1))

## -----------------------------------------------------------------------------
par = variable$new(type = "float", kind = "parameter", value = 0.8)
var1 = variable$new(type = "float", kind = "decision", domain = c(0, par$value))
var2 = variable$new(type = "float", kind = "decision", domain = c(0, par$value))

constr = constraint$new(operator = ">=", variables = c(var1, var2))

## -----------------------------------------------------------------------------
obj <- objective$new(type_of_problem = "satisfy")

## -----------------------------------------------------------------------------
# parameter variable
p1 = variable$new(type = "int", value = 3, kind = "parameter")

# decision variables
v1 = variable$new(type = "int", kind = "decision", domain = c(1, p1$value), 
                  name = "wa")
v2 = variable$new(type = "int", kind = "decision", domain = c(1, p1$value), 
                  name = "nt")
v3 = variable$new(type = "int", kind = "decision", domain = c(1, p1$value), 
                  name = "sa")
v4 = variable$new(type = "int", kind = "decision", domain = c(1, p1$value),
                  name = "q")
v5 = variable$new(type = "int", kind = "decision", domain = c(1, p1$value),
                  name = "nsw")
v6 = variable$new(type = "int", kind = "decision", domain = c(1, p1$value),
                  name = "v")
v7 = variable$new(type = "int", kind = "decision", domain = c(1, p1$value),
                  name = "t")
vars = c(v1, v2, v3, v4, v5, v6, v7)

# constraints
c1 = constraint$new(operator = "!=", variables = c(v1, v2))
c2 = constraint$new(operator = "!=", variables = c(v1, v3))
c3 = constraint$new(operator = "!=", variables = c(v2, v3))
c4 = constraint$new(operator = "!=", variables = c(v2, v4))
c5 = constraint$new(operator = "!=", variables = c(v3, v4))
c6 = constraint$new(operator = "!=", variables = c(v3, v5))
c7 = constraint$new(operator = "!=", variables = c(v3, v6))
c8 = constraint$new(operator = "!=", variables = c(v4, v5))
c9 = constraint$new(operator = "!=", variables = c(v5, v6))
constr = c(c1, c2, c3, c4, c5, c6, c7, c8, c9)

objective_of_problem  = objective$new(type_of_problem = "satisfy")

# create the model
m = model$new(parameter = c(p1), decision = vars, constraints = constr, 
              objective = objective_of_problem)

## -----------------------------------------------------------------------------
solution <- results$new(model = m, result_type = "string")
# show the solution
print(solution$result)

## ----echo=FALSE---------------------------------------------------------------
# remove the temporary model file 
file.remove(solution$mzn)

