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
                             #' @param expression expression to minimize or maximize
                             initialize = function(solve_type, expression = NULL){
                                
                                  assert_choice(solve_type, .globals$objectives)
                                  private$.st = solve_type
                                  
                                  if(test_choice(solve_type, "satisfy")){
                                    assert_null(expression)
                                  }else{
                                    assertR6(expression, "Expression")
                                    private$.exp = expression
                                  }
                                  
                                },
                             #' @description return the expression
                             e =  function(){
                               return(private$.exp)
                             },
                             st = function(){
                               return(private$.st)
                             },
                             #' @description to string method
                             c_str = function(){
                               if(private$.st == "satisfy"){
                                 return(sprintf("solve satisfy;"))
                               }else{
                                 mainExp = private$.exp
                                 if(testR6(mainExp, "Call")){
                                   cl = mainExp
                                   fnId  = cl$id()$id()
                                   iExp = cl$e_i(1)
                                   iterate = ''
                                   if(testR6(iExp, "Comprehension")){
                                     iter = 'i'
                                     iterate = iExp$gen_i(1)$In()$id()
                                   }
                                   cExp = iExp$e()
                                   lId = cExp$lhs()$id()$id()
                                   lAcc = cExp$lhs()$index()$id()
                                   operator = cExp$op()
                                   rId = cExp$rhs()$id()$id()
                                   rAcc = cExp$rhs()$index()$id()
                                   return(sprintf("solve %s %s(%s in %s)(%s[%s] %s %s[%s]);", private$.st,
                                                  fnId, iter, iterate, lId,lAcc, operator, rId, rAcc))
                                 }  
                               }
                               
                             }
                             ),
                          private = list(
                            #' @description .exp
                            #' the expression to maximize or minimize
                            .exp = NULL,
                            #' @description .st
                            #' the solve type
                            .st = NULL
                          )
                    )
