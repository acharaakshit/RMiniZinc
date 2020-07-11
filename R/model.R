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

model = R6Class("model", 
                 public = list(
                 #' @description create a new instance of model class
                 #' @param decls variable declarations of the model 
                 #' @param constraints constraints of the model
                 #' @param objective  type of the problem
                 initialize = function(decls, constraints, objective){
                   
                   assert_list(decls, "VarDeclItem")
                   private$.decls = decls
                   
                   assert_list(constraints, "ConstraintItem")
                   private$.constraints =  constraints
                   
                   assertR6(objective, "SolveItem")
                   private$.solve_type = objective
                 }
                 ),
                private = list(
                  #' @field parameters
                  #' vector of variable declarations of the model
                  .decls = NULL,
                  #' @field constraints
                  #' vector of constraint items of the model
                  .constraints = NULL,
                  #' @field objective
                  #' solve type of the problem
                  .solve_type = NULL
                )
                )
