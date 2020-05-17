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
      }else if(test_atomic_vector(parameters[[i]]$value) && !test_scalar(parameters[[i]]$value)){
        
        if(test_choice(parameters[[i]]$type, "enum")){
          sprintf("%s%s: %s = {%s};\n", code, parameters[[i]]$type,
                  parameters[[i]]$get_name(), paste(as.character(parameters[[i]]$value), collapse=", "))
        }else if(test_choice(parameters[[i]]$type, "set")){
          sprintf("%s%s of %s: %s = %s..%s;\n", code, parameters[[i]]$type, type_of(parameters[[i]]$value[1]),
                  parameters[[i]]$get_name(), parameters[[i]]$value[1], parameters[[i]]$value[2])
        }else if(test_array(parameters[[i]]$type, "array")){
          sprintf("%s%s of %s: %s = {%s};\n", code, parameters[[i]]$type, type_of(parameters[[i]]$value[1]),
                  parameters[[i]]$get_name(), parameters[[i]]$value) 
        }else{
          stop("The parameter value is not a valid MiniZinc type")
        }
      }
    }
    
    decisions = model$decisions
    
    # add definitions for decision variables
    for(i in 1:length(decisions)) {
      code = sprintf("%svar %s..%s: %s;\n", code, decisions[[i]]$domain[1],
                     decisions[[i]]$domain[2], decisions[[i]]$get_name())
    }
    
    constraints = model$constraints
    # add constraints
    for(i in 1:length(constraints)) {
      code = sprintf("%sconstraint %s %s %s;\n", code,
                     constraints[[i]]$variables[[1]]$get_name(), constraints[[i]]$operator,
                     constraints[[i]]$variables[[2]]$get_name())
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

#' @title helper function to get data type 
#' 
#' @description 
#' This function is used to map the data types obtained using typeof()
#' in R to the data types of MiniZinc
#' 
#' @export
#' @param val the value of parameter

type_of <- function(val){
  if(test_choice(typeof(val), "integer")){
    return("int")
  }else if(test_choice(typeof(val), "double")){
    return("float")
  }else if(test_choice(typeof(val), "logical")){
    return("bool")
  }
}
