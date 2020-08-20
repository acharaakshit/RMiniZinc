#' @title init all classes
#' @description given the return value of
#' `mzn_parse()`, it creates a model in R
#' using the API mirror
#' @import rlist
#' @param mznParseList list input
#' @export
getRModel = function(mznParseList){
  items = c()
  if(length(mznParseList) == 1 && names(mznParseList) == "ASSIGNMENTS"){
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
