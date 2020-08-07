# package level environment
#' @importFrom  rlang env
.globals = env()
.globals$Type$kinds =  c("var", "par") 
.globals$Type$baseTypes = c("bool", "int", "float", "string", "unknown", "ann", "bot") 
.globals$binopTypes =  c("+", "-", "!=", "<->", ">=", "<=", "*", ">", "<", "->", "<-", "..", "\\/", "/\\", "'not'",
                          "subset", "superset", "union", "diff", "symdiff", "intersect",
                          "^", "div", "mod", "/", "++", "xor", "in", "=")
.globals$unopTypes = c("+", "-", "!")
.globals$objectives = c("satisfy", "maximize", "minimize")

