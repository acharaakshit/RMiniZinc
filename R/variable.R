#' @title  Declare a new parameter or a new variable.
#'
#' @description 
#' An object containing all the properties of a variable.
#' @import R6
#' @import checkmate
#' @import rlang
#' 
#' @export


variable <- R6Class("variable",
                      public = list(
                      #' @field kind
                      #' specify whether it is a parameter or decision variable     
                      kind = NULL,     
                      #' @field name
                      #' name of the parameter or decision variable
                      name = NULL,
                      #' @field domain
                      #' the domain of decision variable
                      domain = NULL,
                      #' @field type
                      #' data type of the parameter
                      type = NULL,
                      #' @field value
                      #' value of the parameter
                      value = NULL,
                      #' @field expr
                      #' the expression for a decision or parameter
                      expr = NULL,
                      #' @description initialize the variable class
                      #' @param kind parameter or decision variable
                      #' @param name name of the parameter or decision variable
                      #' @param type data type of parameter or decision variable
                      #' @param value value of the parameter or decision variable
                      #' @param domain domain of decision variable
                      #' @param expr  expression value for the decision variable
                      initialize = function(kind, name = NULL, type, value = NULL,
                                            domain = NULL, expr = NULL){
                        
                              assert_choice(kind, .globals$kinds)
                              self$kind = kind
                              
                              assert(test_choice(type, .globals$types$single),
                                     test_choice(type, .globals$types$collection),combine = "or")
                              self$type = type
                              
                              if(test_choice(kind, "decision")){
                                self$domain = domain
                              }
                              
                              if(!is.null(value) && test_choice(kind, "parameter")){
                                self$value = value   
                              }else if(!is.null(expr)){
                                self$expr = expr
                              }
                              if(!is.null(name)){
                                self$name = name
                              }else {
                                # update variable name counts
                                if(self$kind == "parameter") {
                                  private$.static$parameter = private$.static$parameter + 1
                                  private$.name = paste("p", private$.static$parameter, sep = "")
                                }
                                if(self$kind == "decision") {
                                  private$.static$decision = private$.static$decision + 1
                                  private$.name = paste("d", private$.static$decision, sep = "")
                                }
                                self$name <- private$.name
                              }
                              
                        },
                      #' @description
                      #' Returns variable name that is used in Minizinc model.
                      get_name = function() {
                        return(self$name)
                      },
                      
                      #' @description
                      #' Set variable name
                      #' @param name the name to be set
                      set_name = function(name) {
                        private$.name = name
                      }),
                    private = list(
                      #' @field name
                      #' Variable character name for internal use within Minizinc model.
                      .name = NULL,
                      
                      .static = env(parameter = 0, decision = 0)
                    ))
