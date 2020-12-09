#' @title Item class (Abstract)
#' @description 
#' Abstract class for all items in
#' MiniZinc grammar
#' @import R6
#' @import checkmate
#' @export
Item = R6Class("Item",
               public = list(
                 #' @description constructor
                 initialize = function(){
                   stop(paste(RSmisc::getR6Class(self), "can't be initialized."))  
                 }
               ))