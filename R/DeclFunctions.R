# API functions to be exposed to the user

#' @title new int declaration
#' @description declare a new int variable
#' @param name variable/parameter name
#' @param kind "var" or "par"
#' @param e value (NULL by default)
#' @export
IntDecl = function(name, kind, e = NULL){
  parTI = TypeInst$new(Type$new(base_type = "int", kind = kind))
  return(VarDecl$new(id = name, type_inst = parTI, e = e))
}

#' @title new float declaration
#' @description declare a new float
#' @param name variable/parameter name
#' @param kind "var" or "par"
#' @param e value (NULL by default)
#' @export
FloatDecl = function(name, kind, e = NULL){
  parTI = TypeInst$new(Type$new(base_type = "float", kind = kind))
  return(VarDecl$new(id = name, type_inst = parTI, e = e))
}

#' @title new bool declaration
#' @description declare a new bool 
#' @param name variable/parameter name
#' @param kind "var" or "par"
#' @param e value (NULL by default)
#' @export
BoolDecl = function(name, kind, e = NULL){
  parTI = TypeInst$new(Type$new(base_type = "bool", kind = kind))
  return(VarDecl$new(id = name, type_inst = parTI, e = e))
}

#' @title new int set declaration
#' @description declare a new set of int
#' @param name variable/parameter name
#' @param kind "var" or "par"
#' @param e value (NULL by default)
#' @export
IntSetDecl = function(name, kind, e = NULL){
  parTI = TypeInst$new(Type$new(base_type = "int", kind = kind, set_type = TRUE))
  return(VarDecl$new(id = name, type_inst = parTI, e = e))
}


#' @title new 1D int array declaration
#' @description declare a new array of int
#' @param name variable/parameter name
#' @param kind "var" or "par"
#' @param ind index of the array
#' @param e value (NULL by default)
#' @export
IntArrDecl = function(name, kind, ind, e = NULL){
  parTI = TypeInst$new(Type$new(base_type = "int", kind = kind, dim = 1), indexExprVec = ind)
  return(VarDecl$new(id = name, type_inst = parTI, e = e))
}

