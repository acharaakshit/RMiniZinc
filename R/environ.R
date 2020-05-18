# package level environment
.globals = env()
.globals$types = list(single = c("int", "float", "bool"),
                    collection = c(c("array",c(1,2,3,4,5)), "enum" , "set"))
.globals$kinds = c("parameter", "decision")
.globals$constraints = c("==", "!=", "<", ">", ">=", "<=")
.globals$arithmetic_operators = c("+","-","\\","*")
.globals$functions = c("forall", "exists")
.globals$objectives = c("satisfy", "maximize", "minimize")
.globals$result_types = c("string", "R6")
.globals$solvers = c("Gecode")