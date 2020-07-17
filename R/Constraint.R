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
                       #' @param expression The expression for the constraint
                       initialize = function(expression) {
                         assertR6(expression, "Expression")
                         private$.expression = expression
                       },
                       #' @description return the constraint expression
                       e = function(){
                         return(private$.expression)
                       },
                       #' @description serialize to MiniZinc syntax
                       c_str = function(){
                         # currently only one knapsack problem can be solved
                         mainExp = private$.expression
                         return(sprintf("constraint %s;", helper_serialize(mainExp)))
                      }
                     ),
                     private = list(
                       #' @field .expression
                       #' the constraint expression
                       .expression = NULL
                     )
                  )
