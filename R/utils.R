#' @title init all classes
#' @description 
#' Given the return value of
#' `mzn_parse()`, it creates a model in R
#' using the API mirror
#' @import rlist
#' @param mznParseList list input
getRModel = function(mznParseList){
  items = c()
  if("ASSIGNMENTS" %in% names(mznParseList) && !("VARIABLES" %in% names(mznParseList))){
    stop("Variable declarations should be provided with assignment items")
  }
  for (i in seq(1, length(mznParseList), 1)) {
    if(names(mznParseList[i]) != "OUTPUT_ITEM"){
      items = c(items, initItem(mznParseList[i])) 
    }else{
      warning("Output items are not supported! Model will be returned without the output item")
    }
  }
  .globals$help_track_decls = NULL
  if(length(items)){
    mod = Model$new(items = items)
    return(mod) 
  }else{
    stop("No items found -- did you pass just an Output item and/or model string to parse?")
  }
}

#' @title initialize R6 from parsed (not to be exposed)
#' @description 
#' Initialize all the R6 objects
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
        if(variableItems[[j]]$getDecl()$getId()$getName() == parsedList$ASSIGNMENTS[[i]]$NAME){
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
#' @description 
#' Recursive helper function for initilizing
#' expression classes
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
    dims  = c()
    for (i in seq(1, length(pList$ARRAY$DIM_SIZE), 1)) {
      dims = c(dims, IntSetVal$new(imin =  pList$ARRAY$DIM_SIZE[[i]][["MIN"]],
                                  imax = pList$ARRAY$DIM_SIZE[[i]][["MAX"]]))
    }
    expList = list()
    for (i in seq(1, length(pList$ARRAY$ELEMENTS), 1)) {
      expList = list.append(expList, initExpression(pList$ARRAY$ELEMENTS[[i]]))
    }
    return(Array$new(exprVec = expList, dimranges = dims))
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
    # print(names(pList)) # for debugging
    stop("not supported!")
  }
}

#' @title initialized type (not exposed to user)
#' @description  
#' Helper function to initialise the type.
#' @param type_str type string returned by `parse_mzn()`.
#' @param kind par or var
getType = function(type_str, kind){
  if(type_str %in% c("int", "float", "bool", "string", "unknown", "ann")){
    return(Type$new(base_type = type_str, kind = kind))
  }else if(type_str == "set of int"){
    Type$new(base_type = "int", kind = kind, set_type = TRUE)
  }else if(type_str == "set of float"){
    return(Type$new(base_type = "float", kind = kind, set_type = TRUE))
  }else if(type_str == "set of bool"){
    return(Type$new(base_type = "bool", kind = kind, set_type = TRUE))
  }else if(type_str == "set of string"){
    return(Type$new(base_type = "string", kind = kind, set_type = TRUE))
  }else if(type_str == "unknown set"){
    return(Type$new(base_type = "unknown", kind = kind, set_type = TRUE))
  }else if(type_str == "type couldn't be  identified"){
    return(Type$new(base_type = "unknown", kind = kind))
    #stop("variable declaration not supported yet!")
  }else{
    ndim = substring(type_str,1,1)
    ntype_str = substring(type_str, 2)
    if(ntype_str == " dimensional array of int"){
      return(Type$new(base_type = "int", kind = kind, dim = ndim ))
    }else if(ntype_str == " dimensional array of float"){
      return(Type$new(base_type = "float", kind = kind, dim = ndim))
    }else if(ntype_str == " dimensional array of bool"){
      return(Type$new(base_type = "bool", kind = kind, dim = ndim))
    }else if(ntype_str == " dimensional array of string"){
      return(Type$new(base_type = "string", kind = kind, dim = ndim))
    }else if(ntype_str == " dimensional unknown array"){
      return(Type$new(base_type = "unknown", kind = kind, dim = ndim))
    }else if(ntype_str == " dimensional array of set of int"){
      return(Type$new(base_type = "int", kind = kind, dim = ndim, set_type = TRUE))
    }else if(ntype_str == " dimensional array of set of float"){
      return(Type$new(base_type = "float", kind = kind, dim = ndim, set_type = TRUE))
    }else if(ntype_str == " dimensional array of set of bool"){
      return(Type$new(base_type = "bool", kind = kind, dim = ndim, set_type = TRUE))
    }else if(ntype_str == " dimensional array of set of string"){
      return(Type$new(base_type = "string", kind = kind, dim = ndim, set_type = TRUE))
    }else {
      stop("type error -- not indentified")
    }
  }
}


#' @title helper delete item
#' @description 
#' Helper function to search the through a
#' model for an item and return the object
#' if found
#' @param classNm name of the object class
helperDeleteItem = function(classNm){
  for (object in ls(parent.frame(n = 2))) {
    if(testR6(get(object, envir = parent.frame(n = 2)), "Model")){
      mod = get(object)
      mod = itemDelete(classNm, mod)
    }
  }
}

#' @title helper delete expression
#' @description
#' helper function to search the through a
#' model for an expression and return the 
#' object if found
#' @param classNm name of the object class
helperDeleteExpression = function(classNm){
  for (object in ls(parent.frame(n = 2))) {
    if(testR6(get(object, envir = parent.frame(n = 2)), "Model")){
      mod = get(object)
      mod = expressionDelete(classNm, mod)
    }
  }
}

#' @title search item in model and delete
#' @description 
#' Find the object in the model and delete it.
#' @param classNm object to be deleted
#' @param model model to delete the object from
itemDelete = function(classNm, model){
  assertCharacter(classNm)
  for (object in model$getItems()) {
    if(class(object)[1] == classNm){
      if(object$getDeleteFlag()){
        items = model$getItems()
        model$setItems(items[-which(lapply(items, function(x) identical(x, object)) == TRUE)])
        break
      }
    }
  }
}

#' @title delete an expression
#' @description 
#' Delete the object everywhere from the
#' MiniZinc model
#' @param classNm class of the object to delete
#' @param model model to delete the object from
expressionDelete = function(classNm, model){
  assertCharacter(classNm)
  for (object in model$getItems()) {
    for (item in model$getItems()) {
      iterItem(model, classNm)
    }
  }
}

#' @title check all possible items(Under Development)
#' @description 
#' Find the expressions in the items and
#' delete them if matched
#' @param mod model to be searched
#' @param classNm class name of the object to be deleted
iterItem = function(mod, classNm){
  itList = mod$getItems()
  for(item in itList){
    delCount = 0
    if(testR6(item, "VarDeclItem")){
      if(class(item$getDecl())[1] == classNm && item$getDecl()$getDeleteFlag()){
        delCount = delCount + 1
      }else{
          delCount = delCount + iterExpression(classNm, item$getDecl())
      }
      suppressWarnings(mzn_parse(item$c_str()))
    }else if(testR6(item, "ConstraintItem")){
      delCount = 0
      if(class(item$getExp())[1] == classNm && item$getExp()$getDeleteFlag()){
        delCount = delCount + 1
      }else{
          delCount = delCount + iterExpression(classNm, item$getExp())
      }
      suppressWarnings(mzn_parse(item$c_str()))
    }else if(testR6(item, "AssignItem")){
      if(class(item$getValue())[1] == classNm && item$getValue()$getDeleteFlag()){
        delCount = delCount + 1 
      }else{
          delCount = delCount + iterExpression(classNm, item$getValue()) 
      }
      suppressWarnings(mzn_parse(item$c_str()))
    }else if(testR6(item, "SolveItem")){
      if((class(item$getExp())[1] == classNm && item$getExp()$getDeleteFlag()) ||
         (class(item$getAnn())[1] == classNm && item$getAnn()$getDeleteFlag())){
        delCount = delCount + 1
      }else{
        delCount = delCount + iterExpression(classNm, item$getExp())
        if(iterExpression(classNm, item$getAnn())){
          item$setAnn(NULL)
        }
      }
      suppressWarnings(mzn_parse(item$c_str()))
    }else if(testR6(item, "FunctionItem")){
      if((class(item$getBody())[1] == classNm && item$getBody()$getDeleteFlag()) ||
         (class(item$getAnn())[1] == classNm && item$getAnn()$getDeleteFlag())){
        delCount = delCount + 1
      }else{
        setDecl = c()
        for (decl in item$getDecls()) {
         if(iterExpression(classNm, decl)){
           setDecl = c(setDecl, decl)
         }
        }
        if(length(setDecl) == length(item$getDecls())){
          delCount = delCount + 1
        }else if(length(setDecl)){
          item$setDecls(setDecl)
        }
        if(iterExpression(classNm, item$getBody())){
          item$setBody(NULL)
        }
        if(iterExpression(classNm, item$getAnn())){
          item$setAnn(NULL)
        }
      }
      suppressWarnings(mzn_parse(item$c_str()))
    }else if (testR6(item, "IncludeItem")){
      if(classNm == "IncludeItem" && item$getDeleteFlag()){
        delCount = delCount + 1
      }
    }else{
      print(class(item))
      stop("invalid object")
    }
    if(delCount){
      mod$setItems(itList[-which(lapply(itList, function(x) identical(x, item)) == TRUE)])
      itList = itList[-which(lapply(itList, function(x) identical(x, item)) == TRUE)]
    }
  }
}

#' @title iterate through expressions and delete (Under Development)
#' @description 
#' Given an object to delete and expression object,
#' delete all the embedded expression objects that
#' are identical
#' @param classNm class name of the object to delete
#' @param expObj expression object to iterate through
iterExpression = function(classNm, expObj){
  if(testR6(expObj, "VarDecl")){
    if(class(expObj$getValue())[1] == classNm && expObj$getValue()$getDeleteFlag()){
      expObj$setValue(NULL)
    }
    flag = iterExpression(classNm, expObj$getValue())
    if(flag == 1) {
      expObj$setValue(NULL)
    }
    return(0)
  }else if(testR6(expObj, "BinOp")){
    if(iterExpression(classNm, expObj$getLhs()) || iterExpression(classNm, expObj$getRhs())){
      return(1)
    }
    return(0)
  }else if(testR6(expObj, "Id")){
    if(class(expObj)[1] == classNm && expObj$getDeleteFlag()){
      return(1)
    }
    return(0)
  }else if(testR6(expObj, "Set")){
    if(expObj$isEmpty()){
      return(0)
    } 
    elements = expObj$getSetVec()
    del_elements = c()
    for(i in seq(1, elements, 1)){
      if(class(elements[[i]])[1] ==  classNm && elements[[i]]$getDeleteFlag()){
        del_elements = c(del_elements, i)
      }
    }
    if(length(del_elements) == 1 && length(elements) == 1){
      expObj$makeEmpty()
    }else{
      expObj$setSetVec(elements[-del_elements])
    }
    return(0)
  }else if(testR6(expObj, "Array")){
    return(0)
  }else if(testR6(expObj, "ArrayAccess")){
    if(class(expObj$getV())[1] == classNm && expObj$getV()$getDeleteFlag()){
      return(1)
    }
    for(arg in expObj$getArgs()){
      if(class(arg)[1] == classNm && arg$getDeleteFlag()){
        return(1)
      }
    }
    return(0)
  }else if(testR6(expObj, "UnOp")){
    argList = expObj$getArgs() 
    internal_flag = 0
    for(arg in argList){
      if(class(arg)[1] == classNm && arg$getDeleteFlag()){
        expObj$setArgs(argList[-which(lapply(argList, function(x) identical(x, arg)) == TRUE)])
        internal_flag = internal_flag + 1
      }
    }
    if(length(argList) == internal_flag){
      return(1)
    }
    return(0)
  }else if(testR6(expObj, "Generator")){
    if(class(expObj$getIn())[1] == classNm && expObj$getIn()$getDeleteFlag()){
      return(1)
    }else if(class(expObj$getWhere())[1] == classNm && expObj$getWhere()$getDeleteFlag()){
      expObj$setWhere(NULL)
    }
    return(0)
  }else if(testR6(expObj, "Comprehension")){
    gens = expObj$getGens()
    internal_flag = 0
    for(gen in gens){
      if(class(gen)[1] == classNm && gen$getDeleteFlag()){
        expObj$setGens(gens[-which(lapply(gens, function(x) identical(x, gen)) == TRUE)])
        internal_flag = internal_flag + 1
      }else{
        if(iterExpression(classNm, gen) == 1){
          expObj$setGens(gens[-which(lapply(gens, function(x) identical(x, gen)) == TRUE)])
          internal_flag = internal_flag + 1
        }
      }
    }
    if(length(gens) == internal_flag || (class(expObj$getBody())[1] == classNm 
       && expObj$getBody()$getDeleteFlag()) || iterExpression(classNm, expObj$getBody())){
      return(1)
    }
    return(0)
  }else if(testR6(expObj, "Call")){
    argList = expObj$getArgs() 
    internal_flag = 0
    for(arg in argList){
      if(class(arg)[1] == classNm && arg$getDeleteFlag()){
        expObj$setArgs(argList[-which(lapply(argList, function(x) identical(x, arg)) == TRUE)])
        internal_flag = internal_flag + 1
      }else{
        if(iterExpression(classNm, arg) == 1){
          internal_flag = internal_flag + 1
        }
      }
    }
    if(length(argList) == internal_flag){
      return(1)
    }
    return(0)
  }else if(testR6(expObj, "Let")){
    letList = expObj$getLets()
    internal_flag = 0
    for(decl in letList){
      if(class(decl)[1] == classNm && decl$getDeleteFlag()){
        expObj$setLets(letList[-which(lapply(letList, function(x) identical(x, decl)) == TRUE)])
        internal_flag = internal_flag + 1
      }
    }
    if(length(letList) == internal_flag || class(expObj$getBody())[1] == classNm 
       && expObj$getBody()$getDeleteFlag()){
      return(1)
    }
    return(0)
  }else if(testR6(expObj, "Ite")){
    ifList = expObj$getIfs()
    thenList = expObj$getThens()
    return(0)
  }else if(is.null(expObj)) {
    return(0)
  }else if(testR6(expObj, "Annotation")){
    expList = expObj$getExps()
    internalFlag = 0
    for(expr in expList){
      if(class(expr)[1] == classNm && expr$getDeleteFlag()){
        expObj$setExps(expList[-which(lapply(expList, function(x) identical(x, expr)) == TRUE)])
        internalFlag = internalFlag + 1
      }else if(iterExpression(classNm, expr)){
        internalFlag = internalFlag + 1
      }
    }
    if(internalFlag == length(expList)){
      return(1)
    }
    return(0)
  }else{
    assertChoice(class(expObj)[1], c("Int", "Float", "Bool", "String", "TypeInst"))
    return(0)
  }
}
