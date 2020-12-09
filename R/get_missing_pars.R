#' @title get missing parameters
#' @description 
#' Get the values of the missing parameters
#' @param model object of Model class
#' @export
get_missing_pars = function(model){
  assertR6(model, "Model")
  missing_pars = c()
  for(i in seq(1, model$nitems(), 1)){
   if(testR6(model$getItem(i), "VarDeclItem")){
     if(model$getItem(i)$getDecl()$isPar() && is.null(model$getItem(i)$getDecl()$getValue())){
       missing_pars = c(missing_pars, model$getItem(i)$getDecl()$getId()$getName())
     }
   }
  }
  return(missing_pars)
}