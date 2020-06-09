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
#' # or they can be given integer, float or enum ranges using c(l=lower_bound, u = upper_bound)
#' par_setB <- variable$new(kind = "parameter", type = "set", sub_type = "int", value = c(l=1,u=10))
#' # arrays can only be provided "array" type values in R. They can be declared without indices 
#' par_arrrayA <- variable$new(kind = "parameter", type = "array", sub_type = "int", 
#'                value = array(1:50, c(10,10,10,10,10)))
#' # or with given indices that can be sets, enums or integer ranges (can only be a list)
#' par_arrayB <- variable$new(kind = "parameter", type = "array", sub_type = "int", 
#'               value = array(1:50, c(10,10,10,10,10)), array_index = list(c(l=1,u=10),c(l=1,u=10)
#'               ,c(l=1,u=10),c(l=1,u=10),c(l=1,u=10)) )


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
                                # value or expression should be provided 
                                assert(!test_null(value),  !test_null(expr), combine = "or")
                                if(test_null(value)){
                                  assert_r6(expr, "get_expression")
                                  self$expr = expr
                                }else{
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
                                  if(test_choice(self$type , "enum")){
                                    assert(test_character(value), test_atomic_vector(value),
                                           test_null(sub_type), combine = "and")
                                  }
                                }
                              }
                              
                            # checks for decision variables (except sets and arrays)
                            if(test_choice(kind, "decision")){
                                assert_null(value)
                                if(!test_null(domain)){
                                  self$domain = domain
                                }}
                              if(!test_null(expr)){
                                assert_r6(expr, "get_expression")
                                self$expr = expr
                              }
                      
                            # checks for sets and arrays      
                            if(test_choice(self$type , "set")){
                              #set
                                set_checks$new(kind, sub_type, value, expr)
                                self$sub_type = sub_type
                                self$value = value
                            }else if(test_choice(self$type , "array")){
                                #array checks
                                array_checks$new(kind, sub_type, value,
                                                       array_index, expr) 
                                self$sub_type = sub_type
                                self$value = value  
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

#' @title set checks
#' 
#' @description 
#' this class contains the various checks required for sets
#' 
#' @import R6
#' @import checkmate
#' 
#' @export
set_checks <- R6Class("set_checks",
                      public = list(
                        #' @description checks related to set variables and parameters
                        #' @param set_kind parameter or decision
                        #' @param set_type data type of the set
                        #' @param set_value the value of set if it's a parameter
                        #' @param set_expression the input expression
                        initialize = function(set_kind, set_type, set_value, set_expression){
                          if(!testNull(set_expression)){
                            assert_r6(set_expression, "get_expression")
                          }else{
                            if(set_kind == "parameter"){
                              # set parameters can be only of type int, float, bool, enum
                              assert(test_choice(set_type, .globals$sub_types),
                                     !test_choice(set_type , "set"), combine = "and")
                              
                              # if value is not an integer or float ranges
                              if(test_null(names(set_value))){
                                if(test_choice(set_type, c("int", "float", "bool"))){
                                  assert(test_double(set_value), test_logical(set_value), combine = "or")   
                                }else{
                                  # enum
                                  assert(test_r6(set_value, "variable"),
                                         test_choice(set_value$type,"enum"),
                                         combine = "and")
                                }
                              }else{
                                # if the value is a range (integer or float)
                                if(test_character(set_value)){
                                  assert(all.equal(names(set_value),c("l","u", "enum_par")),
                                         test_true(set_value["l"] %in% set_value["enum_par"]$value), 
                                         test_true(set_value["u"] %in% set_value["enum_par"]$value),
                                         combine = "and")
                                }else{
                                  assert(all.equal(names(set_value),c("l","u")),
                                         test_double(set_value["l"]), test_double(set_value["u"]),
                                         combine = "and")
                                }
                              }  
                            }else{
                              assert_choice(set_type, c("int","enum"))
                            }
                            
                          }
                        }
                      ))

#' @title array checks
#' 
#' @description 
#' this class contains the various checks required for array values
#' 
#' @import R6
#' @import checkmate
#' 
#' @export
array_checks <- R6Class("array_checks", 
                       public = list(
                         #' @description this constructor contains the checks for array values
                         #' @param array_kind parameter or decision
                         #' @param array_type int, float, bool, set or enum
                         #' @param array_value value of the array (only for parameters)
                         #' @param array_index index of the array
                         #' @param array_expr expression to be given to the array
                         initialize = function(array_kind, array_type, array_value = NULL,
                                               array_index = NULL, array_expr = NULL){
                        
                           assert(test_choice(array_type, .globals$sub_types),
                                  !test_choice(array_type, "array"), combine = "and")
                           if(test_choice(array_kind, "parameter")){
                             assert(!test_null(array_expr), !test_null(array_value),
                                    combine = "or")
                             assert(test_array(array_value) , test_true(length(dim(array_value)) < 6), 
                                    combine = "and")
                             if(test_choice(array_type, "enum")){
                               assert(test_character(array_value))
                             }else if(test_choice(array_type, "set")){
                               assert(test_double(array_value, "double"))
                             }
                           }else{
                             assert(!test_null(array_index) , test_null(array_value),
                                    combine = "and")
                           }
                           
                           if(!test_null(array_index)){
                             assert(test_list(array_index))
                             if(test_null(array_expr)){
                               assert(length(array_index) == length(dim(array_value)))
                             }
                             # array_index can be  an integer range, a set variable initialised to 
                             # an integer range or an enumeration type
                             assert(all(sapply(array_index, function(x) 
                               if(all.equal(names(x),c("l","u"))){
                                 # for integer ranges  
                                 # add implementation for checking the length of 
                                 # each dimension of array with the index
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
                           }
                        } 
                       ))

