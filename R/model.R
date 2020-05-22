#' @title minizinc model class
#' 
#' @description 
#' This class will take all the objects required to create a MiniZinc model namely, new_param,
#' new_decision_var, new_constraint and specify_problem. 
#' 
#' @import R6
#' @import checkmate
#' 
#' @export


model <- R6Class("model", 
                 public = list(
                 #' @field parameters
                 #' vector of parameter \code{\link{variable}} of the model
                 parameters = NULL,
                 #' @field  decisions
                 #' vector of decision \code{\link{variable}} of the model
                 decisions = NULL,
                 #' @field constraints
                 #' vector of \code{\link{constraint}} of the model
                 constraints = NULL,
                 #' @field objective
                 #' \code{\link{objective}} of the problem
                 objective = NULL,
                 #' @description create a new instance of model class
                 #' @param parameters parameters of the model 
                 #' @param decisions decision variables of the model
                 #' @param constraints constraints of the model
                 #' @param objective  type of the problem
                 initialize = function(parameters = NULL, decisions, constraints, objective){
                   
                   assert(test_list(parameters, "variable"), test_null(parameters), combine = "or")
                   self$parameters = parameters
                   
                   assert_list(decisions, "variable")
                   self$decisions = decisions
                   
                   assert_list(constraints, "constraint")
                   self$constraints =  constraints
                   
                   assert_class(objective, "objective")
                   self$objective = objective
                 }
                 ))
