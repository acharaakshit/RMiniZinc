#' function to generate an expression 
#' @description 
#' This class can be used to store an expression. A simple expression consists of variables and
#' operators. The data structure to be chosen for storing expressions is yet to be decided.
#' 
#' @import R6
#' @export

get_expression = R6Class("get_expression",
                         public = list(
                           #' @description the function to store an expression
                           #' @param variables the variables involved in the expression in order
                           #' @param arithmetic_operator the arithmetic operators involved in the function
                           initialize = function(variables, arithmetic_operator = NULL){
                             
                             if(!testNull(arithmetic_operator)){
                             
                               
                               assert(all(arithmetic_operator %in% .globals$allowed_arithmetic_operators))
                               
                               
                             assert_choice(length(arithmetic_operator), length(variables) - 1)
                             
                             expr = ""
                             
                             for(x in seq(1,length(variables),1)){
                               
                               assert(test_r6(variables[[x]], "variable" ),
                                      test_number(variables[[x]]),
                                      combine = "or")
                               
                               if(test_r6(variables[[x]], "variable" )){
                                if(x == length(variables)){
                                  expr = paste0(expr,variables[[x]]$get_name())
                                }else{
                                  expr = paste0(expr,variables[[x]]$get_name(), arithmetic_operator[[x]])
                                }
                               }else{
                                 if(x == length(variables)){
                                   expr = paste0(expr,variables[[x]])
                                 }else{
                                   expr = paste0(expr,variables[[x]], arithmetic_operator[[x]])
                                 }
                               }
                             }
                             }else{
                               assert(test_r6(variables, "variable"), test_number(variables), combine = "or")
                               if(test_r6(variables, "variable")){
                                  expr = variables$get_name()
                               }else{
                                 expr = as.character(variables)
                               }
                             }
                             
                            private$.expr = expr
                          },
                          #' @description
                          #' Returns the required expression
                          get_expr = function() {
                            return(private$.expr)
                          }
                           ),
                         private = list(
                           #' @field expr
                           #' the generated expression from the inputs
                           .expr = NULL
                         )
                         )
