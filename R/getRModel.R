#' @title init all classes
#' @description given the return value of
#' `mzn_parse()`, it creates a model in R
#' using the API mirror
#' @import rlist
#' @export
#' @param mznParseList list input
getRModel = function(mznParseList){
  items = c()
  for (i in seq(1, length(mznParseList), 1)) {
    items = c(items, initItem(mznParseList[i]))
  }
  mod = Model$new(items = items)
  return(mod)
}

#' @title initialize R6 from parsed (not to be exposed)
#' @description initialize all the R6 objects
#' using the list returned by `mzn_parse()` to
#' create exactly the same structure in R.
#' @import rlist
#' @param parsedList list returned by `mzn_parse()`
initItem = function(parsedList){
  if(is.null(names(parsedList)) || length(names(parsedList)) != 1){
    stop("Supply list as returned by `mzn_parse()`")
  }else if(names(parsedList) == "VARIABLES"){
    vList = parsedList$VARIABLES
    vItems = c()
    for (i in seq(1, length(vList), 1)) {
      dets = vList$DETAILS
      vType = getType(typeStr = vList[[i]]$DETAILS$TYPE, kind = vList[[i]]$DETAILS$KIND )
      indexList = NULL
      if(!is.null(vList[[i]]$DETAILS$INDEX)){
        indexList = list()
        for(j in seq(1, length(vList[[i]]$DETAILS$INDEX), 1)){
          indexList = list.append(indexList, initExpression(vList[[i]]$DETAILS$INDEX[[j]]))
        } 
      }
      ti = TypeInst$new(type = vType, domain = initExpression(vList[[i]]$DETAILS$DOMAIN), 
                        indexExprVec = indexList)
      ob = VarDecl$new(id =  vList[[i]]$DETAILS$NAME, type_inst = ti)
      vItems = c(vItems, VarDeclItem$new(decl = VarDecl$new(id =  vList[[i]]$DETAILS$NAME, type_inst = ti,
                                                            e = initExpression(vList[[i]]$DETAILS$VALUE))))
    }
    return(vItems)
  }else if(names(parsedList) == "CONSTRAINTS"){
    cList = parsedList$CONSTRAINTS
    cItems = c()
    for (i in seq(1, length(cList), 1)) {
      cItems = c(cItems, ConstraintItem$new(e = initExpression(cList[[i]]$DETAILS)))
    }
    return(cItems)
  }else if(names(parsedList) == "SOLVE_TYPE"){
    sit = parsedList$SOLVE_TYPE 
    ann = NULL
    if(!is.null(parsedList$SOLVE_TYPE$DETAILS$ANNOTATION)){
      ann = initExpression(pList = parsedList$SOLVE_TYPE$DETAILS[2]) 
    }
    exp = initExpression(pList = parsedList$SOLVE_TYPE$DETAILS$EXPRESSION)
    return(SolveItem$new(solve_type = sit$OBJECTIVE, e = exp, ann = ann))
  }else if(names(parsedList) == "FUNCTION_ITEMS"){
    
  }else if(names(parsedList) == "ASSIGNMENTS"){

  }else if(names(parsedList) == "MODEL_STRING"){
    # do nothing
  }else{
    stop("Not supported yet")
  }
}



#' @title initExpression (not exposed to the user)
#' 
#' @description recursive helper function for initilizing
#' expression classes
#' 
#' @import R6
#' @import checkmate
#' @import rlist
#' @param pList list from mzn_parse to initialise objects
initExpression = function(pList){
  if(is.null(names(pList))){
    #warning("NULL argument")
    return(NULL)
  }else  if(names(pList) == "BINARY_OPERATION"){
    iLhs = initExpression(pList$BINARY_OPERATION$LHS)
    bOP =  gsub("'", '', pList$BINARY_OPERATION$BINARY_OPERATOR)
    iRhs = initExpression(pList$BINARY_OPERATION$RHS)
    ob = BinOp$new(lhs_expression = iLhs, binop = bOP, rhs_expression = iRhs)
    return(ob)
  }else if(names(pList) == "FUNCTION_CALL"){
    fnId  = pList$FUNCTION_CALL$NAME
    fnArgs = list()
    for (i in seq(1, length(pList$FUNCTION_CALL$ARGUMENTS))) {
      fnArgs = list.append(fnArgs, initExpression(pList[["FUNCTION_CALL"]][["ARGUMENTS"]][[i]]))
    }
    ob = Call$new(fnName = fnId, args = fnArgs)
    return(ob) 
  }else if(names(pList) == "ID"){
    ob = Id$new(pList$ID)
    return(ob)
  }else if(names(pList) == "COMPREHENSION"){
    size = length(pList$COMPREHENSION$GENERATORS)
    genList = list()
    gList = pList$COMPREHENSION$GENERATORS
    for(i in seq(1, size, 1)){
      declList = list()
      for(j in seq(1, length(gList[[i]][["DECLARATIONS"]]), 1)){
        declList = list.append(declList, initExpression(gList[[i]][["DECLARATIONS"]][[j]]))
      }
      genList = list.append(genList, 
                            Generator$new(IN = initExpression(gList[[i]][["IN"]]),
                                          where = initExpression(gList[[i]][["WHERE"]]),
                                          decls = declList))
    }
    st = pList$COMPREHENSION$IS_SET
    ob = Comprehension$new(generators = genList, set = st,
                           e = initExpression(pList$COMPREHENSION$EXPRESSION))
    return(ob)
  }else if(names(pList) == "SET"){
    if(length(names(pList$SET)) == 0){
      return(Set$new(empty_set = TRUE))
    }else if(all(names(pList$SET) == c("IMIN", "IMAX"))){
      return(Set$new(val = IntSetVal$new(imin = pList$SET[["IMIN"]], imax = pList$SET[["IMAX"]])))
    }else if(all(names(pList$SET) == c("FMIN", "FMAX"))){
      return(Set$new(val = FloatSetVal$new(fmin = pList$SET[["FMIN"]], fmax = pList$SET[["FMAX"]])))
    }else{
      expList = list()
      for(i in seq(1, length(pList$SET), 1)){
        expList = list.append(expList, initExpression(pList$SET[[i]]))
      }
      return(Set$new(val = expList)) 
    }
  }else if(names(pList) == "ARRAY_ACCESS"){
    aaArgs = list()
    aList  = pList$ARRAY_ACCESS$ARGUMENTS
    for (i in seq(1, length(aList), 1)) {
      aaArgs = list.append(aaArgs, initExpression(aList[[i]]))
    }
    return(ArrayAccess$new(v = initExpression(pList$ARRAY_ACCESS$NAME), args =  aaArgs))
  }else if(names(pList) == "INT"){
    return(Int$new(value = pList$INT))
  }else if(names(pList) == "VARIABLE_DECLARATION"){
    indexList = NULL
    if(!is.null(pList$VARIABLE_DECLARATION$DETAILS$INDEX)){
      indexList = list()
      for(j in seq(1, length(pList$VARIABLE_DECLARATION$DETAILS$INDEX), 1)){
        indexList = list.append(indexList, initExpression(pList$VARIABLE_DECLARATION$DETAILS$INDEX[[j]]))
      } 
    }
    ti = TypeInst$new(type = Type$new(base_type = pList$VARIABLE_DECLARATION$TYPE,
                                 kind = pList$VARIABLE_DECLARATION$KIND,), 
                      domain = initExpression(pList$VARIABLE_DECLARATION$DOMAIN),
                      indexExprVec = indexList)
    
    return(VarDecl$new(id =  pList$VARIABLE_DECLARATION$NAME, type_inst = ti,
                        e = initExpression(pList$VARIABLE_DECLARATION$VALUE)))
  }else if(names(pList) == "STRING"){
    return(String$new(val = pList$STRING))
  }else if(names(pList) == "LET"){
    declList = list()
    for(j in seq(1, length(pList[[i]][["DECLARATIONS"]]), 1)){
      declList = list.append(declList, initExpression(gList[[i]][["DECLARATIONS"]][[j]]))
    }
  }else if(names(pList) == "ANNOTATION"){
    expList = pList$ANNOTATION
    expObjs = list()
    for (i in seq(1, length(expList), 1)) {
      expObjs = list.append(expObjs, initExpression(expList[[i]]))
    }
    return(Annotation$new(expVec = expObjs))
  }else{
    stop("not supported!")
  }
}

#' @title initialized type (not exposed to user)
#' @description  helper function to initialise the type
#' @param typeStr type string returned by `parse_mzn()`.
#' @param kind par or var
getType = function(typeStr, kind){
  if(typeStr %in% c("int", "float", "bool", "string")){
    return(Type$new(base_type = typeStr, kind =  kind))
  }else if(typeStr == "set of int"){
    Type$new(base_type = "int", kind = kind,set_type = TRUE)
  }else if(typeStr == "set of float"){
    return(Type$new(base_type = "float", kind = kind, set_type = TRUE))
  }else if(typeStr == "set of bool"){
    return(Type$new(base_type = "bool", kind = kind, set_type = TRUE))
  }else if(typeStr == "set of string"){
    return(Type$new(base_type = "string", kind = kind, set_type = TRUE))
  }else if(typeStr == "unknown set"){
    return(Type$new(base_type = "unknown", kind = kind, set_type = TRUE))
  }else if(typeStr == "type couldn't be  identified"){
    return(Type$new(base_type = "unknown", kind = kind))
    #stop("variable declaration not supported yet!")
  }else{
    ndim = substring(typeStr,1,1)
    ntypeStr = substring(typeStr, 2)
    if(ntypeStr == " dimensional array of int"){
      return(Type$new(base_type = "int", kind = kind, dim = ndim ))
    }else if(ntypeStr == " dimensional array of float"){
      return(Type$new(base_type = "float", kind = kind, dim = ndim))
    }else if(ntypeStr == " dimensional array of bool"){
      return(Type$new(base_type = "bool", kind = kind, dim = ndim))
    }else if(ntypeStr == " dimensional array of string"){
      return(Type$new(base_type = "string", kind = kind, dim = ndim))
    }else if(ntypeStr == " dimensional unknown array"){
      return(Type$new(base_type = "unknown", kind = kind, dim = ndim))
    }else {
      stop("type error")
    }
  }
}