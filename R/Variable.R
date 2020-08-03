#' @title The variable declaration item
#' 
#' @description Declaration items in the model
#' 
#' @import R6
#' @import checkmate
#' 
#' @export
VarDeclItem = R6Class("VarDeclItem",
                      inherit = Item,
                      public = list(
                        #' @description constructor
                        #' @param decl the declaration expression object
                        initialize =  function(decl){
                          assertR6(decl, "VarDecl")
                          private$.decl = decl
                        },
                        #' @description get the declaration expression object
                        e = function(){
                          return(private$.decl)
                        },
                        #' @description convert the declaration to String
                        c_str = function(){
                          return(sprintf("%s;", private$.decl$c_str()))
                        }
                      ),
                      private = list(
                        #' @field .declExpression 
                        #' the declaration expression
                        .decl = NULL
                      )
                    )
