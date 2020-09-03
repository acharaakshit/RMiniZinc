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
                   # check if duplicates are present
                   if(length(unique(items))<length(items)){
                     stop("Duplicate items are not allowed!")
                   }
                   # check if more than one solve item is present
                   sI = private$.slvCheck(items)
                   if(sI>1){
                     stop("Only one solve Item is allowed")
                   }else if(sI == 0){
                     warning("Atleast one solve item should be present to evaluate the model")
                   }
                   private$.items = items
                 },
                 #' @description get all the items
                 getItems = function(){
                   return(private$.items)
                 },
                 #' @description set all the items
                 #' @param items items to be set
                 setItems = function(items){
                   assert_list(items, "Item")
                   if(length(unique(items))<length(items)){
                     stop("Duplicate items are not allowed!")
                   }
                   # check if more than one solve item is present
                   sI = private$.slvCheck(items)
                   if(sI>1){
                     stop("Only one solve Item is allowed")
                   }else if(sI == 0){
                     warning("Atleast one solve item should be present to evaluate the model")
                   }
                   private$.items = items
                 },
                 #' @description get the item using index
                 #' @param i index
                 getItem = function(i){
                   return(private$.items[[i]])
                 },
                 #' @description set the item using index
                 #' @param i index
                 #' @param item item to be set
                 setItem = function(i, item){
                   if(testTRUE(class(private$items[[i]])[1] == class(item)[1])){
                     stop("The class of the items should be same")
                   }
                   if(length(unique(items))<length(items)){
                     stop("Duplicate items are not allowed!")
                   }
                    private$.items[[i]] = item
                 },
                 #' @description add item to the model
                 #' @param item item to add
                 addItem = function(item){
                   assertR6(item, "Item")
                   sI = private$.slvCheck(private$.items)
                   if(sI == 1 && assertR6(item, "SolveItem")){
                     stop("Can't add another solve item!")
                   }
                   if(length(unique(items))<length(items)){
                     stop("Duplicate items are not allowed!")
                   }
                   private$.items[[self$nitems() + 1]] = item
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
                 }
                 ),
                private = list(
                  #' @field .items
                  #' list of items in the model
                 .items = NULL,
                 #' @description solve item check
                 #' @param items items passed
                 .slvCheck = function(items){
                   sI = 0
                   for(i in seq(1, length(items), 1)){
                     if(testR6(items[[i]], "SolveItem")){
                       sI = sI+1
                     }
                   }
                   return(sI)
                 }
                ))
