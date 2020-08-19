#' @title initialize R6 from parsed (not to be exposed)
#' @description initialize all the R6 objects
#' using the list returned by `mzn_parse()` to
#' create exactly the same structure in R.
#' @import rlist
#' @param parsedList list returned by `mzn_parse()`
initItem = function(parsedList){
  if(is.null(names(parsedList)) || length(names(parsedList)) != 1){
    stop("Supply list as returned by `mzn_parse()`")
  }else if(names(parsedList) == "INCLUDES"){
    incList = c()
    for (i in seq(1, length(parsedList$INCLUDES), 1)) {
      incList = c(incList, IncludeItem$new(name = parsedList$INCLUDES[[i]]$INCLUDED_MZN))
    }
    return(incList)
  }else if(names(parsedList) == "VARIABLES"){
    vList = parsedList$VARIABLES
    vItems = c()
    for (i in seq(1, length(vList), 1)) {
      ti = initExpression(vList[[i]]$DETAILS["TYPE_INST"])
      vItems = c(vItems, VarDeclItem$new(decl = VarDecl$new(name =  vList[[i]]$DETAILS$NAME, type_inst = ti,
                                                            value = initExpression(vList[[i]]$DETAILS$VALUE))))
    }
    .globals$help_track_decls = vItems 
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
      ann = initExpression(pList = parsedList$SOLVE_TYPE$DETAILS["ANNOTATION"]) 
    }
    exp = initExpression(pList = parsedList$SOLVE_TYPE$DETAILS$EXPRESSION)
    return(SolveItem$new(solve_type = sit$OBJECTIVE, e = exp, ann = ann))
  }else if(names(parsedList) == "FUNCTION_ITEMS"){
    fnList = c()
    for(i in seq(1, length(parsedList$FUNCTION_ITEMS), 1)){
      v_decls = c()
      vDecls = parsedList$FUNCTION_ITEMS[[i]]$DETAILS$DECLARATIONS
      for (j in seq(1, length(vDecls), 1)) {
        ti = initExpression(vDecls[[j]]["TYPE_INST"])
        v_decls = c(v_decls, VarDecl$new(name = vDecls[[j]]$NAME, type_inst = ti,
                                         value = vDecls[[j]]$VALUE))
      }
      fnList = c(fnList, FunctionItem$new(name = parsedList$FUNCTION_ITEMS[[i]]$FUNCTION_NAME,
                                          decls = v_decls, 
                                          rt = initExpression(parsedList$FUNCTION_ITEMS[[i]]$DETAILS["TYPE_INST"]), 
                                          body = initExpression(parsedList$FUNCTION_ITEMS[[i]]$DETAILS$EXPRESSION),
                                          ann = initExpression(parsedList$FUNCTION_ITEMS[[i]]$DETAILS["ANNOTATION"])))
    }
    return(fnList)
  }else if(names(parsedList) == "ASSIGNMENTS"){
    assignList = c()
    variableItems = .globals$help_track_decls
    for (i in seq(1, length(parsedList$ASSIGNMENTS), 1)) {
      for (j in seq(1, length(variableItems), 1)) {
        if(variableItems[[j]]$getDecl()$id()$getId() == parsedList$ASSIGNMENTS[[i]]$NAME){
          aItem  = AssignItem$new(decl = variableItems[[j]]$getDecl(), 
                                  value = initExpression(parsedList$ASSIGNMENTS[[i]]$VALUE))
        }
      }
      assignList = c(assignList, aItem)
    }
    return(assignList)
  }else if(names(parsedList) == "MODEL_STRING"){
    # do nothing
  }else{
    stop("Incorrect named list passed: Please provide the list returned by mzn_parse()")
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
  if(is.null(pList) || is.na(names(pList)) || is.null(names(pList))){
    #warning("NULL argument")
    return(NULL)
  }else if(names(pList) == "INT"){
    return(Int$new(val = pList$INT))
  }else if(names(pList) == "BOOL"){
    return(Bool$new(val = pList$BOOL))
  }else if(names(pList) == "FLOAT"){
    return(Float$new(val = pList$FLOAT))
  }else if(names(pList) == "STRING"){
    return(String$new(val = pList$STRING))
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
  }else  if(names(pList) == "BINARY_OPERATION"){
    iLhs = initExpression(pList$BINARY_OPERATION$LHS)
    bOP =  gsub("'", '', pList$BINARY_OPERATION$BINARY_OPERATOR)
    iRhs = initExpression(pList$BINARY_OPERATION$RHS)
    return(BinOp$new(lhs = iLhs, binop = bOP, rhs = iRhs))
  }else  if(names(pList) == "UNARY_OPERATION"){
    uExpList =  list()
    for (i in seq(1, length(pList$UNARY_OPERATION$ARGUMENTS))) {
      uExpList = list.append(uExpList, initExpression(pList$UNARY_OPERATION$ARGUMENTS[[i]]))
    }
    uOP =  gsub("'", '', pList$UNARY_OPERATION$UNARY_OPERATOR)
    return(UnOp$new(args = uExpList, op = uOP))
  }else if(names(pList) == "FUNCTION_CALL"){
    fnId  = pList$FUNCTION_CALL$NAME
    fnArgs = list()
    for (i in seq(1, length(pList$FUNCTION_CALL$ARGUMENTS))) {
      fnArgs = list.append(fnArgs, initExpression(pList$FUNCTION_CALL$ARGUMENTS[[i]]))
    }
    return(Call$new(fnName = fnId, args = fnArgs)) 
  }else if(names(pList) == "ID"){
    return(Id$new(pList$ID))
  }else if(names(pList) == "COMPREHENSION"){
    size = length(pList$COMPREHENSION$GENERATORS)
    genList = list()
    gList = pList$COMPREHENSION$GENERATORS
    for(i in seq(1, size, 1)){
      declList = list()
      for(j in seq(1, length(gList[[i]]$DECLARATIONS), 1)){
        declList = list.append(declList, initExpression(gList[[i]]$DECLARATIONS[[j]]))
      }
      genList = list.append(genList, 
                            Generator$new(IN = initExpression(gList[[i]]$IN),
                                          where = initExpression(gList[[i]]$WHERE),
                                          decls = declList))
    }
    st = pList$COMPREHENSION$IS_SET
    return(Comprehension$new(generators = genList, set = st,
                             body = initExpression(pList$COMPREHENSION$EXPRESSION)))
  }else if(names(pList) == "LET"){
    letList = list()
    for(i in seq(1, length(pList$LET$LET_EXPRESSION), 1)){
      letList = list.append(letList, initExpression(pList$LET$LET_EXPRESSION[[i]]))
    }
    return(Let$new(let = letList, body = initExpression(pList$LET$IN_EXPRESSION)))
  }else if(names(pList) == "ANNOTATION"){
    expList = pList$ANNOTATION
    expObjs = list()
    for (i in seq(1, length(expList), 1)) {
      expObjs = list.append(expObjs, initExpression(expList[[i]]))
    }
    return(Annotation$new(expVec = expObjs))
  }else if(names(pList) == "ARRAY"){
    expList = list()
    for (i in seq(1, length(pList$ARRAY), 1)) {
      expList = list.append(expList, initExpression(pList$ARRAY[[i]]))
    }
    return(Array$new(exprVec = expList))
  }else if(names(pList) == "IF_THEN_ELSE"){
    ifList = list()
    for (i in seq(1, length(pList$IF_THEN_ELSE$IF), 1)) {
      ifList = list.append(ifList, initExpression(pList$IF_THEN_ELSE$IF[[i]]))
    }
    thenList = list()
    for (i in seq(1, length(pList$IF_THEN_ELSE$THEN), 1)) {
      thenList = list.append(thenList, initExpression(pList$IF_THEN_ELSE$THEN[[i]]))
    }
    return(Ite$new(ifs = ifList, thens = thenList, Else = initExpression(pList$IF_THEN_ELSE$ELSE)))
  }else if(names(pList) == "VARIABLE_DECLARATION"){
    
    ti = initExpression(pList$VARIABLE_DECLARATION["TYPE_INST"])
    
    return(VarDecl$new(name =  pList$VARIABLE_DECLARATION$NAME, type_inst = ti,
                       value = initExpression(pList$VARIABLE_DECLARATION$VALUE)))
  }else if(names(pList) == "TYPE_INST"){
    indexList = NULL
    if(!is.null(pList$TYPE_INST$INDEX)){
      indexList = list()
      for(j in seq(1, length(pList$TYPE_INST$INDEX), 1)){
        if(names(pList$TYPE_INST$INDEX[[j]]) == "UNRESTRICTED"){
          indexList = list.append(indexList, "int") 
        }else{
          indexList = list.append(indexList, initExpression(pList$TYPE_INST$INDEX[[j]]))
        }
      } 
    }
    return(TypeInst$new(type = getType(pList$TYPE_INST$TYPE,
                                       kind = pList$TYPE_INST$KIND), 
                        domain = initExpression(pList$TYPE_INST$DOMAIN),
                        indexExprVec = indexList))
  }else{
    print(names(pList))
    stop("not supported!")
  }
}

#' @title initialized type (not exposed to user)
#' @description  helper function to initialise the type
#' @param typeStr type string returned by `parse_mzn()`.
#' @param kind par or var
getType = function(typeStr, kind){
  if(typeStr %in% c("int", "float", "bool", "string", "unknown", "ann")){
    return(Type$new(base_type = typeStr, kind = kind))
  }else if(typeStr == "set of int"){
    Type$new(base_type = "int", kind = kind, set_type = TRUE)
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
    }else if(ntypeStr == " dimensional array of set of int"){
      return(Type$new(base_type = "int", kind = kind, dim = ndim, set_type = TRUE))
    }else if(ntypeStr == " dimensional array of set of float"){
      return(Type$new(base_type = "float", kind = kind, dim = ndim, set_type = TRUE))
    }else if(ntypeStr == " dimensional array of set of bool"){
      return(Type$new(base_type = "bool", kind = kind, dim = ndim, set_type = TRUE))
    }else if(ntypeStr == " dimensional array of set of string"){
      return(Type$new(base_type = "string", kind = kind, dim = ndim, set_type = TRUE))
    }else {
      stop("type error -- not indentified")
    }
  }
}