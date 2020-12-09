#' @title get ints
#' @description 
#' Get a list of integer expressions
#' @param vals vector of integer values
#' @export
intExpressions = function(vals){
  lapply(vals, function(val) assert_true(val - floor(val) == 0))
  retVals = c()
  for(val in vals){
    retVals = c(retVals, Int$new(val))
  }
  return(retVals)
}

#' @title get floats
#' @description 
#' Get a list of floats expressions
#' @param vals vector of floats values
#' @export
floatExpressions = function(vals){
  assert_numeric(vals)
  retVals = c()
  for(val in vals){
    retVals = c(retVals, Float$new(val))
  }
  return(retVals)
}

#' @title get bools
#' @description 
#' Get a list of bool expressions
#' @param vals vector of bool values
#' @export
boolExpressions = function(vals){
  assert_logical(vals)
  retVals = c()
  for(val in vals){
    retVals = c(retVals, Bool$new(val))
  }
  return(retVals)
}

#' @title get strings
#' @description 
#' Get a list of string expressions
#' @param vals vector of string values
#' @export
stringExpressions = function(vals){
  assert_character(vals)
  retVals = c()
  for(val in vals){
    retVals = c(retVals, String$new(val))
  }
  return(retVals)
}
