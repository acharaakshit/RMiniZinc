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
                         inherit = Item,
                     public = list(
                       #' @description
                       #' Creates a new instance of Constraint class.
                       #' @param e The expression for the constraint
                       initialize = function(e) {
                         assertR6(e, "Expression")
                         private$.e = e
                       },
                       #' @description return the constraint expression
                       e = function(){
                         return(private$.e)
                       },
                       #' @description serialize to MiniZinc syntax
                       c_str = function(){
                         return(sprintf("constraint %s;", private$.e$c_str()))
                      }
                     ),
                     private = list(
                       #' @field .expression
                       #' the constraint expression
                       .e = NULL
                     )
                  )
