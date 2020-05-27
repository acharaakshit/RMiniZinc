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
                      #' @field sub_type
                      #' the data type of elements of a collection type
                      sub_type = NULL,
                      #' @description initialize the variable class
                      #' @param kind parameter or decision variable
                      #' @param name name of the parameter or decision variable
                      #' @param type data type of parameter or decision variable
                      #' @param value value of the parameter or decision variable
                      #' @param domain domain of decision variable
                      #' @param expr  expression value for the decision variable
                      #' @param sub_type the data type of element of collections
                      initialize = function(kind, name = NULL, type, value = NULL,
                                            domain = NULL, expr = NULL, sub_type = NULL){
                        
                              # kind checks
                              assert_choice(kind, .globals$kinds)
                              self$kind = kind
                              
                              # type checks
                              assert(test_choice(type, .globals$types$single),
                                     test_choice(type, .globals$types$collection),combine = "or")
                              self$type = type
                              
                              # value checks for parameters
                              if(test_choice(kind, "parameter")){
                                assert(!test_null(value))
                              if(test_choice(self$type, .globals$types$single)){
                                assert_scalar(value)
                                assert_null(sub_type)
                                if(test_choice(self$type, "int")){
                                  assert(all(value == floor(value)))
                                }else if(test_choice(self$type, "float")){
                                  assert_choice(typeof(value), "double")
                                }else{
                                  assert_choice(typeof(value), "logical")
                                }
                                self$value = value
                              }
                              
                              if(test_choice(type, .globals$types$collection)){
                                if(test_choice(self$type , "enum")){
                                  assert(test_character(value),
                                         !test_scalar(value), test_null(sub_type), combine = "and")
                                }else if(test_choice(self$type , "set")){
                                  assert(test_choice(sub_type, .globals$sub_types),
                                         !test_choice(sub_type , "set"), combine = "and")
                                  assert(!test_scalar(value), test_choice(typeof(value), "double"),
                                         test_atomic_vector(value), combine = "and")
                                  self$sub_type = sub_type
                                }else{
                                  assert(test_choice(sub_type, .globals$sub_types),
                                         !test_choice(sub_type, "array"), combine = "and")
                                  assert(test_array(value) , test_true(length(dim(value)) < 6), 
                                         combine = "and")
                                  if(test_choice(sub_type, "enum")){
                                    assert(test_choice(typeof(value), "character"))
                                  }else if(test_choice(sub_type, "set")){
                                    assert(test_choice(typeof(value), "double"))
                                  }
                                  self$sub_type = sub_type
                                }  
                                self$value = value
                              }
                              
                              }
                              
                              if(test_choice(kind, "decision")){
                                if(!test_null(domain)){
                                  self$domain = domain
                                }
                                if(test_choice(type, .globals$types$collection) &&
                                   !test_choice(type, "enum")){
                                  if(test_choice(type,"set")){
                                    assert_choice(sub_type, c("int","enum"))
                                  }else{
                                    assert(test_choice(sub_type, .globals$sub_types),
                                           !test_choice(sub_type, "array"), combine = "and")
                                  }
                                  self$sub_type = sub_type
                                }
                                
                                assert_null(value)
                                if(!test_null(expr)){
                                  assert_r6(expr, "get_expression")
                                  self$expr = expr;
                                }
                                
                                
                              }
                              
                              if(!is.null(expr)){
                                # expression checks (to be done)
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


