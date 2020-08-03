# package level environment
.globals = rlang::env()
.globals$Type$kinds =  c("decision", "parameter") 
.globals$Type$baseTypes = c("bool", "int", "float", "UNKNOWN") 
.globals$binopTypes =  c( "+", "-", ">=",  "<=", "*", ">", "<", "->", "<-", ".." )
.globals$unopTypes = c("+", "-", "!")
.globals$objectives = c("satisfy", "maximize", "minimize")

