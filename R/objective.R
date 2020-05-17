#' @title 
#' mention if the problem is satisfaction, minimization or maximization.
#' 
#' @description 
#' This is a class that will be used to specify whether the optimization problem is a satisfaction,
#' minimization or maximization problem
#' 
#' @import R6
#' @import checkmate
#' 
#' 
#' @export

objective <- R6Class("objective",
                           public = list(
                             #' @field type_of_problem
                             #' mention the type of problem
                             type_of_problem = NULL,
                             #' @field arithmetic_expression
                             #' set the arithmetic expression
                             arithmetic_expression = NULL,
                             
                             #' @description 
                             #' create an instance of specify_problem class
                             #' @param type_of_problem satisfaction, minimization or maximization
                             #' @param arithmetic_expression expression to minimize or maximize
                             initialize = function(type_of_problem, arithmetic_expression = NULL){
                                
                                  assert_choice(type_of_problem, .globals$objectives)
                                  self$type_of_problem = type_of_problem
                                  
                                  assert(test_choice(type_of_problem, "satisfy"), 
                                         test_null(arithmetic_expression), combine = "and")
                                  
                                  if(testTRUE(!test_choice(type_of_problem, "satisfy")) && 
                                         testTRUE(!test_null(arithmetic_expression))){
                                    self$arithmetic_expression = arithmetic_expression  
                                  }else{
                                    assert_null(arithmetic_expression)
                                  }
                                  
                                  
                                  
                                  
                                  }))