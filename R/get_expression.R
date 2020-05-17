#' function to generate an expression 
#' @description 
#' This class can be used to store an expression. A simple expression consists of variables and
#' operators. The data structure to be chosen for storing expressions is yet to be decided.
#' 
#' @import R6
#' @export



get_expression <- R6Class("get_expression",
                         public = list(
                           #'@field Mat 
                           #'This is a matrix containing the variable names
                           Mat = NULL,
                           #' @description the function to store an expression
                           initialize = function(){
                             
                           }
                           )
                         )