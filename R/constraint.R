#' @title Constraint Class
#'
#' @description
#' Constraint is a class to describe Minizinc constraints on decision variables.
#' 
#'
#' @import R6
#' @import checkmate
#'
#' @export

ConstraintItem = R6Class("ConstraintItem",
                     public = list(
                       #' @description
                       #' Creates a new instance of Constraint class.
                       #' @param expression The expression for the constraint
                       initialize = function(expression) {
                         assertR6(expression, "Expression")
                         private$.expression = expression
                       },
                       #' @description return the constraint expression
                       e = function(){
                         return(private$.expression)
                       }
                     ),
                     private = list(
                       #' @field .expression
                       #' the constraint expression
                       .expression = NULL
                     )
                  )
