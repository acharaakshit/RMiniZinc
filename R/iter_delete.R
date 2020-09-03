#' @title search item in model and delete
#' @description find the object in the model and delete it.
#' @param obj object to be deleted
item_delete = function(obj){
  assertR6(obj, "Item")
  for (object in ls(parent.frame(n = 2))) {
    if(testR6(get(object, envir = parent.frame(n = 2)), "Model")){
      mod = get(object)
      for (i in seq(1, mod$nitems(), 1)) {
        if(identical(mod$getItem(i), obj)){
          # remove the item from the model
          # delete the associated assignment if declaration
          # item is removed
          items = mod$getItems()
          mod$setItems(items[-i])
          message("Model updated successfully!")
          break
        }
      }
    }
  }
}

#' @title delete an expression(Under Development)
#' @description delete the object everywhere from the MiniZinc model
#' @param obj object to delete
expression_delete = function(obj){
  assertR6(obj, "Expression")
  for (object in ls(parent.frame(n = 2))) {
    if(testR6(get(object, envir = parent.frame(n = 2)), "Model")){
      mod = get(object)
      for (item in mod$getItems()) {
        iter_item(mod, obj, item)
      }
    }
  }
}

#' @title check all possible items(Under Development)
#' @description find the expressions in the items and delete them if matched
#' @param mod model to be searched
#' @param obj object to be deleted
#' @param item item to be searched
iter_item = function(mod, obj, item){
  itList = mod$getItems()
  if(testR6(item, "VarDeclItem")){
    if(identical(obj, item$getDecl())){
      mod$setItems(itList[-which(lapply(itList, function(x) identical(x, item)) == TRUE)])
      message("deleted associated VarDeclItem")
      return()
    }else{
      iter_expression(obj, item$getDecl())
    }
    suppressWarnings(mzn_parse(item$c_str()))
  }else if(testR6(item, "ConstraintItem")){
    if(identical(obj, item$getExp())){
      mod$setItems(itList[-which(lapply(itList, function(x) identical(x, item)) == TRUE)])
      return()
    }else{
      if(iter_expression(obj, item$getExp()) ==  1){
        mod$setItems(itList[-which(lapply(itList, function(x) identical(x, item)) == TRUE)])
        return()
      }
    }
    suppressWarnings(mzn_parse(item$c_str()))
  }else if(testR6(item, "AssignItem")){
    if(identical(obj, item$getValue())){
      mod$setItems(itList[-which(lapply(itList, function(x) identical(x, item)) == TRUE)])
    }else{
      iter_expression(obj, item$getValue())
    }
    suppressWarnings(mzn_parse(item$c_str()))
  }else if(testR6(item, "SolveItem")){
    # to be done
    #mzn_parse(item$c_str())
  }else if(testR6(item, "FunctionItem")){
    # to be done
    #mzn_parse(item$c_str())
  }else{
    stop("invalid object")
  }
}

#' @title iterate through expressions and delete (Under Development)
#' @description given an object to delete and expression
#' object, delete all the embedded expression objects that
#' are identical
#' @param obj object to delete
#' @param expObj expression object to iterate through
iter_expression = function(obj, expObj){
  if(testR6(expObj, "VarDecl")){
    if(identical(expObj$getValue(), obj)){
      expObj$setValue(NULL)
    }
    flag = iter_expression(obj, expObj$getValue())
    if(flag == 1) {
      expObj$setValue(NULL)
    }
    return(0)
  }else if(testR6(expObj, "BinOp")){
    if(iter_expression(obj, expObj$getLhs()) || iter_expression(obj, expObj$getRhs())){
      return(1)
    }
    return(0)
  }else if(testR6(expObj, "Id")){
    if(identical(expObj, obj)){
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
      if(identical(elements[[i]], obj)){
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
    if(identical(expObj$v(), obj)){
      return(1)
    }
    for(i in seq(1, expObj$nargs(), 1)){
      if(identical(expObj$getIndex(i), obj)){
        return(1)
      }
    }
    return(0)
  }else if(testR6(expObj, "UnOp")){
    argList = expObj$getArgs() 
    internal_flag = 0
    for(arg in argList){
      if(identical(arg, obj)){
        expObj$setArgs(argList[-which(lapply(argList, function(x) identical(x, arg)) == TRUE)])
        internal_flag = internal_flag + 1
      }
    }
    if(length(argList) == internal_flag){
      return(1)
    }
    return(0)
  }else if(testR6(expObj, "Generator")){
    if(identical(expObj$getIn(), obj)){
      return(1)
    }else if(identical(expObj$getWhere(), obj)){
      expObj$setWhere(NULL)
    }
    return(0)
  }else if(testR6(expObj, "Comprehension")){
    gens = expObj$getGens()
    internal_flag = 0
    for(gen in gens){
      if(identical(gen, obj)){
        expObj$setGens(gens[-which(lapply(gens, function(x) identical(x, gen)) == TRUE)])
        internal_flag = internal_flag + 1
      }else{
        if(iter_expression(obj, gen) == 1){
          expObj$setGens(gens[-which(lapply(gens, function(x) identical(x, gen)) == TRUE)])
          internal_flag = internal_flag + 1
        }
      }
    }
    if(length(gens) == internal_flag || identical(expObj$getBody(), obj)){
      return(1)
    }
    return(0)
  }else if(testR6(expObj, "Call")){
    argList = expObj$getArgs() 
    internal_flag = 0
    for(arg in argList){
      if(identical(arg, obj)){
        expObj$setArgs(argList[-which(lapply(argList, function(x) identical(x, arg)) == TRUE)])
        internal_flag = internal_flag + 1
      }else{
        if(iter_expression(obj, arg) == 1){
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
      if(identical(decl, obj)){
        expObj$setLets(letList[-which(lapply(letList, function(x) identical(x, decl)) == TRUE)])
        internal_flag = internal_flag + 1
      }
    }
    if(length(letList) == internal_flag || identical(expObj$getBody(), obj)){
      return(1)
    }
    return(0)
  }else if(testR6(expObj, "Ite")){
    ifList = expObj$getIfs()
    thenList = expObj$getThens()
    return(0)
  }else if(is.null(expObj)) {
    return(0)
  }else{
    assertChoice(class(expObj)[1], c("Int", "Float", "Bool", "String", "Annotation", "TypeInst"))
    return(0)
  }
}
