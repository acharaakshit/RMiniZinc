#' @title class for variable declaration
#' 
#' @description 
#' Contains different fields to create a variable declaration
#' 
#' @import R6 
#' @import checkmate
#' 
#' @export
VarDecl = R6Class("VarDecl",
                   inherit = Expression,
                   public = list(
                     #' @description initialize the VarDecl constructor
                     #' @param type the type object
                     #' @param expression the object of a data type in MiniZinc. NULL by default
                     #' @param kind parameter or decision
                     #' @param id the id/name of the declared variable
                     initialize = function(expression = NULL, id, type_inst){
                       assertR6(type_inst, "TypeInst")
                       private$.ti = type_inst
                       if(testR6(expression, "Expression")){
                         private$.expression  =  expression 
                       }
                       assert_string(id)
                       private$.id = Id$new(id)
                     },
                    #' @description 
                    #' the identifier
                    id = function(){
                      return (private$.id)
                    },
                    #' @description check if it's a parameter
                    isPar = function(){
                      if(private$.ti$type()$kind() == "parameter"){
                        return (TRUE)
                      }
                      return(FALSE)
                    },
                    #' @description check if it's a decision variable
                    isVar = function(){
                      if(private$.ti$type()$kind() == "variable"){
                        return (TRUE)
                      }
                      return(FALSE)
                    },
                    #' @description return the initialization expression
                    e = function(){
                      return(private$.expression)
                    },
                    #' @description type of the variable declaration
                    ti = function(){
                      return(private$.ti)
                    }
                   ),
                   private = list(
                     #' @field .ti
                     #' type instantiation information
                     .ti = NULL,
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
                          id = private$.decl$id()$id()
                          bt = private$.decl$ti()$type()$bt()
                          if(bt == "INT"){
                            t = "int"
                          }
                          if(private$.decl$ti()$type()$ndim() == 1 && !private$.decl$ti()$type()$isSet()){
                            # one dimensional array
                            access_id = ''
                            ti = private$.decl$ti()
                            ind = ti$ranges()
                            if(testR6(ind,"Id")){
                                access_id = ind$id()
                            }else if(testR6(ind, "SetVal")){
                              if(test_numeric(ind$isv()[['l']]) && testR6(ind$isv()[['u']], "Id")){
                                access_id = paste0(ind$isv()[['l']],"..", ind$isv()[['u']]$id())  
                              }else{
                                stop('Not supported')
                              }
                            }else{
                              stop('Incorrect expression')
                            }
                            if(private$.decl$isPar()){
                              return(sprintf("array[%s] of %s: %s;", access_id, t, id)) 
                            }else{
                              dom = ''
                              if(testR6(ti$domain(), "SetVal")){
                                if(test_numeric(ti$domain()$isv()[['l']]) && test_numeric(ti$domain()$isv()[['u']])){
                                  dom = paste0(ti$domain()$isv()[['l']],"..", ti$domain()$isv()[['u']])  
                                }else{
                                  stop('Not supported')
                                }
                              }
                              if(dom != ''){
                                return(sprintf("array[%s] of var %s: %s;", access_id, dom, id))
                              }
                              return(sprintf("array[%s] of var %s: %s;", access_id, t, id))
                            }  
                          }else if(private$.decl$ti()$type()$ndim() == 1 && private$.decl$ti()$type()$isSet()){
                            val = private$.decl$e()$v()$isv()
                            if(private$.decl$isPar()){
                              return(sprintf("set of %s: %s = %s..%s;", t, id, val[["l"]], val[["u"]]$id()))
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
                        #' the declaration expression
                        .decl = NULL
                      )
                    )
