# package level environment
.globals = env()
.globals$types = list(single = c("int", "float", "bool"),
                    collection = c("array", "enum" , "set"))
.globals$sub_types = c("int", "float", "bool", "enum", "set")
.globals$kinds = c("parameter", "decision")
.globals$constraints = c("==", "!=", "<", ">", ">=", "<=")
.globals$allowed_arithmetic_operators = c("+","-","\\","*")
.globals$constraint_connectors = c("/\\", "\\/", "->", "<->", "not" )
.globals$functions = c("forall", "exists")
.globals$objectives = c("satisfy", "maximize", "minimize")
.globals$result_types = c("string", "R6")
.globals$solvers = c("Gecode")
