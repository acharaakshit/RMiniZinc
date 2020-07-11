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
                       private$.id = id
                     },
                    #' @description 
                    #' the identifier
                    id = function(){
                      return (private$.id)
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
                    },
                    #' @description type of the variable declaration
                    type = function(){
                      return(private$.type)
                    }
                   ),
                   private = list(
                     #' @field .type
                     #' type information
                     .type = NULL,
                     #' @field id
                     #' name of the variable
                     .id = NULL,
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
                        },
                        #' @description convert the declaration to String
                        c_str = function(){
                          id = private$.decl$id()$id()
                          bt = private$.decl$type()$bt()
                          if(bt == "INT"){
                            t = "int"
                          }
                          if(private$.decl$type()$ndim() == 1 && !private$.decl$type()$isSet()){
                            # one dimensional array
                            access_id = ''
                            if(!is.null(private$.decl$type()$ti())){
                              ti = private$.decl$type()$ti()
                              ind = ti$ranges()
                              if(testR6(ind,"Id")){
                                access_id = ind$id()
                              }
                            }
                            if(private$.decl$isPar()){
                              return(sprintf("array[%s] of %s: %s;", access_id, t, id)) 
                            }else{
                              return(sprintf("array[%s] of var %s: %s;", access_id, t, id))
                            }  
                          }
                          if(private$.decl$isPar()){
                            return(paste0(t, ": ", id, ";")) 
                          }else{
                            return(paste0("var ", t, ": ", id, ";"))
                          }
                        }
                      ),
                      private = list(
                        #' @field .declExpression 
                        #' @description the declaration expression
                        .decl = NULL
                      )
                    )
