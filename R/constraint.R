#' @title constraint Class
#'
#' @description
#' Constraint is a class to describe Minizinc constraints on variables.
#' It contains information that is needed to define a Minizinc constraint, such as
#' type of constraint (==, !=, >, <, >=, <=) and variables involved in a given constraint.
#'
#' @import R6
#' @import checkmate
#' @import rlang
#'
#' @export

constraint = R6Class("constraint",
                     public = list(
                       #' @field operator
                       #' Constraints specify the Boolean expressions that the decision variables
                       #' must satisfy to be a valid solution to the model.
                       #' For now, only relational operators are supported:
                       #' ==, !=, <, >, <=, >=.
                       #'
                       #' Taken from Minizinc documentation.
                       operator = NULL,
                       #' @field variables
                       #' A list of objects of
                       #' \code{\link{variable}} class.
                       variables = NULL,
                       #' @field expr
                       #' expression for the constraint
                       expr = NULL,
                       #' @description
                       #' Creates a new instance of Constraint class.
                       #' @param operator a constraint of model
                       #' @param variables  the variables involved in the constraint
                       initialize = function(operator, variables) {
                         assert_choice(operator, .globals$constraints)
                         self$operator = operator
                         
                         assert_list(variables, "variable" )
                         self$variables = variables
                         
                         private$.static$constraint = private$.static$constraint + 1
                         private$.name = paste("c", private$.static$constraint, sep = "")
                       },
                       
                       #' @description
                       #' Returns constraint name that is used in Minizinc
                       get_name = function() {
                         return(private$.name)
                       }
                     ),
                     
                     private = list(
                       #' @field name
                       #' Constraint character name for internal use within Minizinc model.
                       .name = NULL,
                       
                       .static = env(constraint = 0)
                     )
                     
)
