#' @title helper_serialize (not exposed to the user)
#' 
#' @description recursive helper function for serializing
#' constraints and solve items to MiniZinc syntax
#' 
#' @import R6
#' @import checkmate
#' 
#' @param Exp the expression/value to serialize
helper_serialize = function(Exp){
  if(testR6(Exp, "Binop")){
    iLhs = helper_serialize(Exp$lhs())
    bOP = Exp$op()
    iRhs = helper_serialize(Exp$rhs())
    return(sprintf("%s %s %s",iLhs,bOP, iRhs))
  }else if(testR6(Exp, "Call")){
    fnId  = helper_serialize(Exp$id())
    iExp = helper_serialize(Exp$e_i(1))
    return(sprintf("%s %s", fnId, iExp)) 
  }else if(testR6(Exp, "Id")){
    return(sprintf("%s", Exp$id()))
  }else if(testR6(Exp, "Comprehension")){
    size = Exp$ngens()
    for(i in seq(1, size, 1)){
      iter = helper_serialize(Exp$gen_i(i)$iter_id())
      iterate = helper_serialize(Exp$gen_i(i)$In())
    }
    cExp = helper_serialize(Exp$e())
    return(sprintf("(%s in %s) (%s)", iter, iterate, cExp))
  }else if(testR6(Exp, "SetVal")){
    if(test_numeric(Exp$isv()[['l']]) &&
       testR6(Exp$isv()[['u']], "Id")){
      return(paste0(Exp$isv()[['l']],"..",helper_serialize(Exp$isv()[['u']])))
    }else{
      stop('Not supported')
    }
  }else if(testR6(Exp, "ArrayAccess")){
    arr_name = helper_serialize(Exp$id())
    access_id = helper_serialize(Exp$index())
    return(sprintf("%s[%s]", arr_name, access_id))
  }else if(testR6(Exp, "Int")){
    return(sprintf("%s", Exp$getIntVal()$v()))
  }else{
    stop("incorrect boy")
  }
}