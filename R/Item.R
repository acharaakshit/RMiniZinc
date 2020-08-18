#' @title Item class (Abstract)
#' @description abstract class for all items of MiniZinc
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