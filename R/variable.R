#' @title class for variable declaration
#' 
#' @description 
#' Converts a data type into a variable declaration
#' 
#' @import R6 
#' @import checkmate
#' 
#' @export
VarDecl <- R6Class("VarDecl",
                   inherit = Expression,
                   public = list(
                     #' @description initialize the VarDecl constructor
                     #' @param type the type object
                     #' @param expression the object of a data type in MiniZinc. NULL by default
                     #' @param kind parameter or decision
                     #' @param id the id/name of the declared variable
                     initialize = function(type, expression = NULL, id){
                       assertR6(type, "Type")
                       private$.type = type
                       if(testR6(expression, "Expression")){
                         private$.expression  =  expression 
                       }
                       assertR6(id, "Id")
                       private$id = id
                     },
                    #' @description 
                    #' 
                    get_id = function(){
                      return (private$id)
                    },
                    #' @description check if it's a parameter
                    isPar = function(){
                      if(private$.type$kind() == "parameter"){
                        return (TRUE)
                      }
                      return(FALSE)
                    },
                    #' @description check if it's a decision variable
                    isVar = function(){
                      if(private$.type$kind() == "variable"){
                        return (TRUE)
                      }
                      return(FALSE)
                    },
                    #' @description return the initialization expression
                    e = function(){
                      return(private$.expression)
                    }
                   ),
                   private = list(
                     #' @field .type
                     #' type information
                     .type = NULL,
                     #' @field id
                     #' name of the variable
                     id = NULL,
                     #' @field .expression
                     #' the initialization expression
                     .expression = NULL
                   ))

#' @title The variable declaration item
#' 
#' @description Declaration items in the model
#' 
#' @import R6
#' @import checkmate
#' 
#' @export
VarDeclItem = R6Class("VarDeclItem",
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
                        }
                      ),
                      private = list(
                        #' @field .declExpression 
                        #' @description the declaration expression
                        .decl = NULL
                      )
                    )
