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
    
    parameters = model$parameters
    
    for(i in 1:length(parameters)) {
      # if the value is a scalar i.e int, float or logical
      if(test_scalar(parameters[[i]]$value)){
      code = sprintf("%s%s: %s = %s;\n", code, parameters[[i]]$type,
                     parameters[[i]]$get_name(), parameters[[i]]$value)
      }else{
        
        if(test_choice(parameters[[i]]$type, "enum")){
          enum_values <- paste(parameters[[i]]$value, collapse=", ")
          sprintf("%s%s: %s = {%s};\n", code, parameters[[i]]$type,
                  parameters[[i]]$get_name(), enum_values)
        }else if(test_choice(parameters[[i]]$type, "set")){
          set_values <- paste0(parameters[[i]]$value, collapse = ",")
          sprintf("%s%s of %s: %s = {%s};\n", code, parameters[[i]]$type, parameters[[i]]$sub_type,
                  parameters[[i]]$get_name(), set_values)
        }else if(test_array(parameters[[i]]$type, "array")){
          
          # this is the number of dimensions of array
          n <- length(dim(parameters[[i]]$value))
          dim_sizes <- ""
          for(x in seq(1,n,1)){
            dim_sizes <- paste0(dim_sizes, dim(parameters[[i]]$value)[x], ", ")
          }
          array_values <- as.vector(parameters[[i]]$value)
          array_indices  = paste0(dim(parameters[[i]]$value), collapse = ",")
          sprintf("%s%s[%s] of %s: %s = array%sd(%s%s);\n", code, parameters[[i]]$type, array_indices,
                  parameters[[i]]$sub_type, parameters[[i]]$get_name(), n, dim_sizes, array_values) 
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
          # to be done
        }else{
          # to be done
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

