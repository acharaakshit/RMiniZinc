#' @title MiniZinc Model class
#' 
#' @description 
#' This class will take all the objects required to create a MiniZinc model.
#' 
#' @import R6
#' @import checkmate
#' 
#' @export

Model = R6Class("Model", 
                 public = list(
                 #' @description create a new instance of model class
                 #' @param items all items of the model 
                 initialize = function(items){
                   assert_list(items, "Item")
                   private$.items = items
                 },
                 #' @description get the item using index
                 #' @param i index
                 item_i = function(i){
                   return(private$.items[[i]])
                 },
                 #' @description get the string representation of the model
                 mzn_string = function(){
                   mzn_str = ''
                   for( i in seq(1,length(private$.items),1)) {
                     mzn_str = paste0(mzn_str, private$.items[[i]]$c_str(), "\n")
                   }
                   return(mzn_str)
                 } 
                 ),
                private = list(
                  #' @field parameters
                  #' vector of variable declarations of the model
                 .items = NULL
                )
                )

#' @title Item class (Abstract)
#' 
#' @description abstract class for all items of MiniZinc
#' 
#' @import R6
#' @import checkmate
#' 
#' @export
Item = R6Class("Item",
               public = list(
                 #' @description constructor
                 initialize = function(){
                   stop(paste(RSmisc::getR6Class(self), "can't be initialized."))  
                 }
               ))