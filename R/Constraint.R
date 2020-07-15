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
                         mainExp = private$.expression
                         if(testR6(mainExp, "Binop")){
                           bop = private$.expression
                           iLhs = bop$lhs()
                           cl = iLhs
                           assertR6(cl, "Call")
                           fnId  = cl$id()$id()
                           iExp = cl$e_i(1)
                           iterate = ''
                           if(testR6(iExp, "Comprehension")){
                             iter = iExp$gen_i(1)$iter_id()$id()
                             iterate = iExp$gen_i(1)$In()$id()
                           }
                           cExp = iExp$e()
                           lId = cExp$lhs()$id()$id()
                           assert_character(lId)
                           lAcc = cExp$lhs()$index()$id()
                           assert_character(lAcc)
                           operator = cExp$op()
                           rId = cExp$rhs()$id()$id()
                           rAcc = cExp$rhs()$index()$id()
                           lhs_str = sprintf("%s(%s in %s)(%s[%s] %s %s[%s])",fnId, iter, iterate,
                                   lId,lAcc, operator, rId, rAcc)
                           iRhs = bop$rhs()$id()
                           bOP = bop$op()
                           return(sprintf("constraint %s %s %s;",lhs_str,bOP, iRhs))
                         }else if(testR6(mainExp, "Call")){
                           cl = mainExp
                           fnId  = cl$id()$id()
                           iExp = cl$e_i(1)
                           iterate = ''
                           if(testR6(iExp, "Comprehension")){
                               iter = iExp$gen_i(1)$iter_id()$id()
                               iterate = iExp$gen_i(1)$In()$id()
                           }
                            cExp = iExp$e()
                            lId = cExp$lhs()$id()$id()
                            lAcc = cExp$lhs()$index()$id()
                            operator = cExp$op()
                            rId = cExp$rhs()$getIntVal()$v()
                            return(sprintf("constraint %s(%s in %s)(%s[%s] %s %s);",fnId, iter, iterate,
                                           lId,lAcc, operator, rId))
                            }
                          
                       }
                     ),
                     private = list(
                       #' @field .expression
                       #' the constraint expression
                       .expression = NULL
                     )
                  )
