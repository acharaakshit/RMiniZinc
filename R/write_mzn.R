#' @title write the model to mzn file
#' 
#' @description 
#' write all the components of the minizinc model to the 
#' temporary mzn file
#' 
#' @export
#' @param model an object of the model class.
#' @param mzn the path of temporary model.

write_mzn <- function(model, mzn){
    code = ""
    
    if(!testNull(model$parameters)){
      
      parameters = model$parameters
      
      for(i in 1:length(parameters)) {
        # if the value is a scalar i.e int, float or logical
        if(test_scalar(parameters[[i]]$value) && test_choice(parameters[[i]]$type,.globals$types$single)){
          code = sprintf("%s%s: %s = %s;\n", code, parameters[[i]]$type,
                         parameters[[i]]$get_name(), 
                         ifelse(is.null(parameters[[i]]$value), parameters[[i]]$value, parameters[[i]]$expr))
        }else{
          if(test_choice(parameters[[i]]$type, "enum")){
            # enum
            if(!is.null(parameters[[i]]$value)){
              # if the enum has a value
              enum_values <- paste(parameters[[i]]$value, collapse=", ")
              sprintf("%s%s: %s = {%s};\n", code, parameters[[i]]$type,
                      parameters[[i]]$get_name(), enum_values)
            }else{
              # if the enum has an expression
              sprintf("%s%s: %s = %s;\n", code, parameters[[i]]$type,
                      parameters[[i]]$get_name(), parameters[[i]]$expr)
            }
          }else if(test_choice(parameters[[i]]$type, "set")){
            # set
            if(!is.null(parameters[[i]]$value)){
              # if the set has a value
              if(!test_null(names(parameters[[i]]$value))){
                # integer, float or enum range
                sprintf("%s%s of %s: %s = %s..%s;\n", code, parameters[[i]]$type, parameters[[i]]$sub_type,
                        parameters[[i]]$get_name(), parameters[[i]]$value["l"], parameters[[i]]$value["u"] )
              }else{
                # set literals
                set_values <- paste0(parameters[[i]]$value, collapse = ",")
                sprintf("%s%s of %s: %s = {%s};\n", code, parameters[[i]]$type, parameters[[i]]$sub_type,
                        parameters[[i]]$get_name(), set_values)
                }
            }else{
              # if the set has an expression
              sprintf("%s%s of %s: %s = %s;\n", code, parameters[[i]]$type, parameters[[i]]$sub_type,
                      parameters[[i]]$get_name(), parameters[[i]]$expr)
            }
          }else{
            # array
            assert_choice(parameters[[i]]$type, "array")
            # value
            array_values <- as.vector(parameters[[i]]$value)
            # the dimensions
            n <- length(dim(parameters[[i]]$value))
            
            if(test_null(parameters[[i]]$array_index)){
              # if array index is not given
              dim_sizes <- paste0("1..", dim(parameters[[i]]$value), collapse = ", ")
              # indices 
              array_indices  <- dim_sizes
              
            }else{
              # if array index is given
              indices_n_dims <- sapply(parameters[[i]]$array_index, function(x) 
                if(all.equal(names(x),c("l","u"))){
                  # integer range
                  list(array_indices = paste0(x["l"],"..",x["u"]), 
                       dim_sizes =  paste0(x["l"],"..",x["u"]))
                }else{
                   # set or enum
                   list(array_indices = x$get_name, dim_sizes = paste0("1..", length(x$value)))
                  })
              array_indices = paste0(indices_n_dims["array_indices",], collapse = ",")
              dim_sizes = paste0(indices_n_dims["dim_sizes",], collapse = ",")
                
            }
            if(is.null(array_values)){
              # if the value was not provided to the parameter
              sprintf("%s%s[%s] of %s: %s = %s ;\n", code, parameters[[i]]$type, array_indices,
                      parameters[[i]]$sub_type, parameters[[i]]$get_name(), parameters[[i]]$expr)
            }else{
              # if the value was provided to the array
              sprintf("%s%s[%s] of %s: %s = array%sd(%s%s);\n", code, parameters[[i]]$type, array_indices,
                    parameters[[i]]$sub_type, parameters[[i]]$get_name(), n, dim_sizes, array_values)    
            }
          }
        }
      }
    }
    
    decisions = model$decisions
    
    # add definitions for decision variables
    for(i in 1:length(decisions)) {
      if(test_null(decisions[[i]]$domain) || test_choice(decisions[[i]]$type, "enum")){
        if(test_choice(decisions[[i]]$type, .globals$types$single)){
          code = sprintf("%svar %s: %s;\n", code, decisions[[i]]$type,
                       decisions[[i]]$get_name())  
        }else if(test_choice(decisions[[i]]$type, "set")){
          # set
          code = sprintf("%svar %s of %s: %s;\n", code, decisions[[i]]$type, decisions[[i]]$sub_type,
                         decisions[[i]]$get_name())  
        }else{
          # array
          indices_n_dims <- sapply(decisions[[i]]$array_index, function(x) 
            if(all.equal(names(x),c("l","u"))){
              # integer range
              list(array_indices = paste0(x["l"],"..",x["u"]))
            }else{
              # set or enum
              list(array_indices = x$get_name)
            })
          array_indices = paste0(indices_n_dims["array_indices",], collapse = ",")
          sprintf("%s%s[%s] of %s: %s;\n", code, decisions[[i]]$type, array_indices,
                  parameters[[i]]$sub_type, decisions[[i]]$get_name())
        }
      }else{
        if(test_choice(decisions[[i]]$type, .globals$types$single)){
            code = sprintf("%svar %s..%s: %s;\n", code, decisions[[i]]$domain[1],
                              decisions[[i]]$domain[2], decisions[[i]]$get_name())
        }
      }
    }
    
    
    constraints = model$constraints
    # add constraints
    for(i in 1:length(constraints)) {
      code = sprintf("%sconstraint %s %s %s;\n", code,
                     constraints[[i]]$LHS_expression, constraints[[i]]$operator,
                     constraints[[i]]$RHS_expression)
    }
    
    objective = model$objective
    if(test_null(objective$arithmetic_expression)){
      code = sprintf("%ssolve %s;\n",code, objective$type_of_problem)
    }else{
      code = sprintf("%ssolve %s %s;\n",code, objective$type_of_problem,
                     objective$arithmetic_expression)
    }
    write(code,mzn)
}

