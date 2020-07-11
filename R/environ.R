# package level environment
.globals = rlang::env()
.globals$Type$kinds =  c("decision", "parameter") 
.globals$Type$baseTypes = c("BOOL", "INT", "FLOAT", "UNKNOWN") 
.globals$Type$setTypes = c("PLAIN", "SET")
.globals$binopTypes =  c(
  PLUS = "+",
  MINUS = "-",
  GEQ = ">=",
  LEQ = "<=",
  MULT = "*"
)
.globals$objectives = c("satisfy", "maximize", "minimize")

