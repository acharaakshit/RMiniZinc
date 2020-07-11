#' @title constraint Class
#'
#' @description
#' Constraint is a class to describe Minizinc constraints on decision variables.
#' 
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
                       #' @field LHS_expression
                       #' left hand side expression for the constraint
                       LHS_expression = NULL,
                       #' @field RHS_expression
                       #' right hand side expression for the constraint
                       RHS_expression = NULL,
                       #' @field fn
                       #' name of the inbuild function to be used.
                       fn = NULL,
                       #' @description
                       #' Creates a new instance of Constraint class.
                       #' @param operator a constraint of model
                       #' @param LHS_expression the left hand side expression in boolean expression
                       #' @param RHS_expression the right hand side expression 
                       #' @param fn_attributes the name, var to iterate on and boolean expression
                       #'  of inbuilt function to use.
                       initialize = function(operator, LHS_expression, RHS_expression, fn_attributes = NULL) {
                         
                         assert_choice(operator, .globals$constraints)
                         self$operator = operator
                         
                         assert(test_r6(LHS_expression,"get_expression"), test_r6(RHS_expression,"get_expression"),
                                combine = "and")
                         
                         self$LHS_expression = LHS_expression$get_expr()
                         self$RHS_expression = RHS_expression$get_expr()
                         
                         if(!test_null(fn_attributes)){
                           assert_choice(fn_attributes$name, .globals$functions)
                           fn = iterator_fnc()
                         }
                         
                         private$.static$constraint = private$.static$constraint + 1
                         private$.name = paste("c", private$.static$constraint, sep = "")
                       },
                       
                       #' @description 
                       #' use the builtin function forall (currently in development)
                       #' @param iterate_on the varaible to be traversed.
                       #' @param bool_expr the boolean expression inside the loop.
                       #' @param name_fn the name of function to be used. "forall" by default
                       iterator_fnc = function(iterate_on, bool_expr, name_fn = "forall"){
                         assert(name_fn, .globals$functions)
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
