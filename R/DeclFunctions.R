# API functions to be exposed to the user

#' @title new int declaration
#' @description declare a new int variable
#' @param name variable/parameter name
#' @param kind "var" or "par"
#' @param value value (NULL by default)
#' @export
IntDecl = function(name, kind, value = NULL){
  parTI = TypeInst$new(Type$new(base_type = "int", kind = kind))
  return(VarDecl$new(name, parTI, value))
}

#' @title new float declaration
#' @description declare a new float
#' @param name variable/parameter name
#' @param kind "var" or "par"
#' @param value value (NULL by default)
#' @export
FloatDecl = function(name, kind, value = NULL){
  parTI = TypeInst$new(Type$new(base_type = "float", kind = kind))
  return(VarDecl$new(name, parTI, value))
}

#' @title new bool declaration
#' @description declare a new bool 
#' @param name variable/parameter name
#' @param kind "var" or "par"
#' @param value value (NULL by default)
#' @export
BoolDecl = function(name, kind, value = NULL){
  parTI = TypeInst$new(Type$new(base_type = "bool", kind = kind))
  return(VarDecl$new(name, parTI, value))
}

#' @title new int set declaration
#' @description declare a new set of int
#' @param name variable/parameter name
#' @param kind "var" or "par"
#' @param value value (NULL by default)
#' @export
IntSetDecl = function(name, kind, value = NULL){
  parTI = TypeInst$new(Type$new(base_type = "int", kind = kind, set_type = TRUE))
  return(VarDecl$new(name, parTI, value))
}

#' @title new set of float declaration
#' @description declare a new set of float
#' @param name variable/parameter name
#' @param kind "var" or "par"
#' @param value value of the set (or NULL)
#' @export
FloatSetDecl = function(name, kind, value = NULL){
  parTI = TypeInst$new(Type$new(base_type = "float", kind = kind, set_type = TRUE))
  return(VarDecl$new(name, parTI, value))
}

#' @title new set of bool declaration
#' @description declare a new set of bool
#' @param name variable/parameter name
#' @param kind "var" or "par"
#' @param value value of the set (or NULL)
#' @export
BoolSetDecl = function(name, kind, value = NULL){
  parTI = TypeInst$new(Type$new(base_type = "bool", kind = kind, set_type = TRUE))
  return(VarDecl$new(name, parTI, value))
}

#' @title new set of string declaration
#' @description declare a new set of string
#' @param name variable/parameter name
#' @param kind "var" or "par"
#' @param value value of the set (or NULL)
#' @export
StringSetDecl = function(name, kind, value = NULL){
  parTI = TypeInst$new(Type$new(base_type = "string", kind = kind, set_type = TRUE))
  return(VarDecl$new(name, parTI, value))
}

#' @title new nD int array declaration
#' @description declare a new array of int
#' @param name variable/parameter name
#' @param kind "var" or "par"
#' @param ind index of the array
#' @param value value (NULL by default)
#' @param ndim number of dimensions of the array
#' @export
IntArrDecl = function(name, kind, ind, value = NULL, ndim){
  parTI = TypeInst$new(Type$new(base_type = "int", kind = kind, dim = ndim), indexExprVec = ind)
  return(VarDecl$new(name, parTI, value))
}

#' @title new nD float array declaration
#' @description declare a new array of float
#' @param name variable/parameter name
#' @param kind "var" or "par"
#' @param ind index of the array
#' @param value value (NULL by default)
#' @param ndim number of dimensions of the array
#' @export
FloatArrDecl = function(name, kind, ind, value = NULL, ndim){
  parTI = TypeInst$new(Type$new(base_type = "float", kind = kind, dim = ndim), indexExprVec = ind)
  return(VarDecl$new(name, parTI, value))
}  


#' @title new nD bool array declaration
#' @description declare a new array of bool
#' @param name variable/parameter name
#' @param kind "var" or "par"
#' @param ind index of the array
#' @param value value (NULL by default)
#' @param ndim number of dimensions of the array
#' @export
BoolArrDecl = function(name, kind, ind, value = NULL, ndim){
  parTI = TypeInst$new(Type$new(base_type = "bool", kind = kind, dim = ndim), indexExprVec = ind)
  return(VarDecl$new(name, parTI, value))
}

#' @title new nD bool array declaration
#' @description declare a new array of bool
#' @param name variable/parameter name
#' @param kind "var" or "par"
#' @param ind index of the array
#' @param value value (NULL by default)
#' @param ndim number of dimensions of the array
#' @export
StringArrDecl = function(name, kind, ind, value = NULL, ndim){
  parTI = TypeInst$new(Type$new(base_type = "bool", kind = kind, dim = ndim), indexExprVec = ind)
  return(VarDecl$new(name, parTI, value))
}



