#' @title get missing parameters
#' @description get the values of the missing parameters
#' @param model object of Model class
#' @export
get_missing_pars = function(model){
  assertR6(model, "Model")
  missing_pars = c()
  for(i in seq(1, model$nitems(), 1)){
   if(testR6(model$item_i(i), "VarDeclItem")){
     if(model$item_i(i)$getDecl()$isPar() && is.null(model$item_i(i)$getDecl()$getValue())){
       missing_pars = c(missing_pars, model$item_i(i)$getDecl()$id()$getId())
     }
   }
  }
  return(missing_pars)
}