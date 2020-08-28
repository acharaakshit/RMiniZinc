#' @title MiniZinc Model class
#' @description 
#' This class will take all the objects required to create a MiniZinc model.
#' @import R6
#' @import checkmate
#' @export
Model = R6Class("Model", 
                 public = list(
                 #' @description create a new instance of model class
                 #' @param items all items of the model 
                 initialize = function(items){
                   assert_list(items, "Item")
                   # check if more than one solve item is present
                   sI = 0
                   for(i in seq(1, length(items), 1)){
                     if(testR6(items[[i]], "SolveItem")){
                       sI = sI+1
                     }
                   }
                   if(sI>1){
                     stop("Only one solve Item is allowed")
                   }else if(sI == 0){
                     warning("Atleast one solve item should be present to evaluate the model")
                   }
                   private$.items = items
                 },
                 #' @description get the item using index
                 #' @param i index
                 item_i = function(i){
                   return(private$.items[[i]])
                 },
                 #' @description get the number of items
                 nitems = function(){
                   return(length(private$.items))
                 },
                 #' @description get the string representation of the model
                 mzn_string = function(){
                   mzn_str = ''
                   for( i in seq(1,length(private$.items),1)) {
                     mzn_str = paste0(mzn_str, private$.items[[i]]$c_str(), "\n")
                   }
                   return(mzn_str)
                 },
                 #' @description delete item i from the model
                 #' @param i index of the item
                 delete = function(i){
                   private$.items[[-i]]
                 }
                 ),
                private = list(
                  #' @field .items
                  #' list of items in the model
                 .items = NULL
                ))
