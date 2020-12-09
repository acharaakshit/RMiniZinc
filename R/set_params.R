#' @title set missing parameters
#' @description 
#' Assign values to parameters which don't have
#' a value assigned yet. 
#' @param model Model object
#' @param modData list of the value objects to be assigned
#' @export
set_params = function(model, modData){
  assertR6(model, "Model")
  assertList(modData, "Expression")
  missing_pars = get_missing_pars(model)
  assertTRUE(length(unique(names(modData))) == length(missing_pars))
  for(name in names(modData)){
    if(!(name %in% missing_pars)){
      stop("incorrect name of list elements")
    }
  }
  modData = modData[match(missing_pars, names(modData))]
  count = 1
  for(i in seq(1, model$nitems(), 1)){
    item = model$getItem(i)
    if(testR6(item, "VarDeclItem")){
      if(item$getId()$getName() == names(modData)[count]){
        item$getDecl()$setValue(modData[[count]])
        count = count + 1
      }
      if(count > length(missing_pars)){
        break
      }
    }
  }
  return(model)
}