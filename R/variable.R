#' @title  Declare a new parameter or a new variable.
#'
#' @description 
#' An object containing all the properties of a variable.
#' @import R6
#' @import checkmate
#' @import rlang
#' 
#' @export
#' @examples 
#' # an integer parameter varaible 
#' par_int <- variable$new(kind = "parameter", type = "int", value = 10)
#' # a floating point decision variable 
#' var_float <- variable$new(kind = "decision", type = "float" )
#' # enum value can only be an atmoic vector of characters
#' par_enum <- variable$new(kind = "parameter", type = "enum", value = c("RED","YELLOW","GREEN"))   
#' # set value can either be an atomic vector of integers or floats 
#' par_setA <- variable$new(kind = "parameter", type = "set", sub_type = "float", value = c(1,3,4.5,6))
#' # or they can be given integer, floating point or enum ranges using c(l=lower_bound, u = upper_bound)
#' par_setB <- variable$new(kind = "parameter", type = "set", sub_type = "int", value = c(l=1,u=10))
#' # arrays can only be provided "array" type values in R. They can be declared without indices 
#' par_arrrayA <- variable$new(kind = "parameter", type = "array", sub_type = "int", 
#'                             value = array(1:50, c(10,10,10,10,10)))
#' # or with given indices that can be sets, enums or integer ranges (can only be a list)
#' par_arrayB <- variable$new(kind = "parameter", type = "array", sub_type = "int", 
#'                            value = array(1:10), array_index = list(c(l=1,u=10),c(l=1,u=10)
#'                            ,c(l=1,u=10),c(l=1,u=10),c(l=1,u=10)) )


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
                      #' @field array_index
                      #' the index values if the variable is of type array
                      array_index = NULL,
                      #' @description initialize the variable class
                      #' @param kind parameter or decision variable
                      #' @param name name of the parameter or decision variable
                      #' @param type data type of parameter or decision variable
                      #' @param value value of the parameter or decision variable
                      #' @param domain domain of decision variable
                      #' @param expr  expression value for the decision variable
                      #' @param sub_type the data type of element of collections
                      #' @param array_index the index if variable is an array (can only be a list)
                      initialize = function(kind, name = NULL, type, value = NULL,
                                            domain = NULL, expr = NULL, sub_type = NULL,
                                            array_index = NULL){
                        
                              # kind checks
                              assert_choice(kind, .globals$kinds)
                              self$kind = kind
                              
                              # type checks
                              assert(test_choice(type, .globals$types$single),
                                     test_choice(type, .globals$types$collection),combine = "or")
                              self$type = type
                              
                              # value checks for parameters
                              if(test_choice(kind, "parameter")){
                                assert(!test_null(value), !testNull(expr), combine = "or")
                              if(testNull(expr)){
                                if(test_choice(self$type, .globals$types$single)){
                                  assert_scalar(value)
                                  assert_null(sub_type)
                                  if(test_choice(self$type, "int")){
                                    assert_int(value)
                                  }else if(test_choice(self$type, "float")){
                                    assert_double(value)
                                  }else{
                                    assert_logical(value)
                                  }
                                  self$value = value
                                }
                                
                                if(test_choice(type, .globals$types$collection)){
                                  if(test_choice(self$type , "enum")){
                                    assert(test_character(value), test_atomic_vector(value),
                                           test_null(sub_type), combine = "and")
                                  }else if(test_choice(self$type , "set")){
                                    # set parameters can be only of type int, float, bool, enum
                                    assert(test_choice(sub_type, .globals$sub_types),
                                           !test_choice(sub_type , "set"), combine = "and")
                                    
                                    # if value is not an integer or float ranges
                                    if(test_null(names(value))){
                                      if(test_choice(sub_type, c("int", "float", "bool"))){
                                        assert(test_double(value), test_logical(value), combine = "and")   
                                      }else{
                                        assert(test_r6(value, "variable"),
                                               test_choice(value$type,"enum"),
                                               combine = "and")
                                      }
                                    }else{
                                      # if the value is a range (integer or float)
                                      if(test_character(value)){
                                        assert(all.equal(names(value),c("l","u", "enum_par")),
                                               test_true(value["l"] %in% value["enum_par"]$value), 
                                               test_true(value["u"] %in% value["enum_par"]$value),
                                               combine = "and")
                                      }else{
                                        assert(all.equal(names(value),c("l","u")),
                                              test_double(value["l"]), test_double(value["u"]),
                                              combine = "and")
                                      }
                                    }
                                    self$sub_type = sub_type
                                  }else{
                                    assert(test_choice(sub_type, .globals$sub_types),
                                           !test_choice(sub_type, "array"), combine = "and")
                                    assert(test_array(value) , test_true(length(dim(value)) < 6), 
                                           combine = "and")
                                    if(test_choice(sub_type, "enum")){
                                      assert(test_character(value))
                                    }else if(test_choice(sub_type, "set")){
                                      assert(test_double(value, "double"))
                                    }
                                    self$sub_type = sub_type
                                  }  
                                  self$value = value
                                }   
                              }else{
                                assert_r6(expr, "get_expression")
                                self$expr = expr;
                              }
                            }
                              
                              # checks for decision variables
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
                                if(!test_null(expr)){
                                    assert_r6(expr, "get_expression")
                                    self$expr = expr;
                                  }
                              }
                                
                                #check for array_index
                                if(!test_null(array_index)){
                                  if(test_choice(kind, "parameter")){
                                    assert_array(value)
                                  }
                                  
                                  assert(test_list(array_index),
                                         test_true(length(array_index) < 6 && length(array_index) > 0),
                                         combine = "and")
                                  # array_index can be  an integer range, a set variable initialised to 
                                  # an integer range or an enumeration type
                                  assert(all(sapply(array_index, function(x) 
                                    if(all.equal(names(x),c("l","u"))){
                                      # for integer ranges  
                                      assert(all(sapply(x, test_int)))
                                      TRUE
                                  }else{
                                      assert_list(x, "variable")
                                      if(x$type == "set"){
                                        # set of int
                                        assert(x$sub_type == "int")
                                      }else{
                                        # enum
                                        assert(x$type == "enum")
                                      }
                                    TRUE
                                  })))
                                  
                                self$array_index = array_index 
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
                      #' variable character name for internal use within Minizinc model.
                      .name = NULL,
                      
                      .static = env(parameter = 0, decision = 0)
                    ))


