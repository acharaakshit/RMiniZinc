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

SolveItem = R6Class("SolveType",
                    inherit = Item,
                           public = list(
                             #' @description 
                             #' create an instance of specify_problem class
                             #' @param solve_type satisfaction, minimization or maximization
                             #' @param e expression to minimize or maximize
                             initialize = function(solve_type, e = NULL){
                                
                                  assert_choice(solve_type, .globals$objectives)
                                  private$.st = solve_type
                                  
                                  if(test_choice(solve_type, "satisfy")){
                                    assert_null(expression)
                                  }else{
                                    assertR6(e, "Expression")
                                    private$.e = e
                                  }
                                  
                                },
                             #' @description return the expression
                             e =  function(){
                               return(private$.e)
                             },
                             st = function(){
                               return(private$.st)
                             },
                             #' @description to string method
                             c_str = function(){
                               if(private$.st == "satisfy"){
                                 return(sprintf("solve satisfy;"))
                               }else{
                                 return(sprintf("solve %s %s;", private$.st, private$.e$c_str()))
                               }
                             }
                             ),
                          private = list(
                            #' @description .e
                            #' the expression to maximize or minimize
                            .e = NULL,
                            #' @description .st
                            #' the solve type
                            .st = NULL
                          )
                    )
