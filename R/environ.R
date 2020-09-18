# package level environment
.globals = env()
.globals$Type$kinds =  c("var", "par") 
.globals$Type$baseTypes = c("bool", "int", "float", "string", "unknown", "ann") 
.globals$binopTypes =  c("+", "-", "!=", "<->", ">=", "<=", "*", ">", "<", "->", "<-", "..", "\\/", "/\\", "'not'",
                          "subset", "superset", "union", "diff", "symdiff", "intersect",
                          "^", "div", "mod", "/", "++", "xor", "in", "=")
.globals$unopTypes = c("+", "-", "not")
.globals$objectives = c("satisfy", "maximize", "minimize")
.globals$help_track_decls = NULL
