#' Declare a new parameter for the MiniZinc model.
#' @export
#' @param model an instance of the temporary mzn file
#' @param par_name the name of the parameter to initialize
#' @param par_type data type of the variable. it can be one of c("int", "float", "bool", "enum", "set", "array" )
#' @param single_par_val value of the parameter where par can be "int" , "float" or "bool"
#' @param enum_val A vector containing the values of enum
#' @param set_val A vector containing the lower and upper limits of a set like c(l, u)
#' @param set_type The data type of the set
#' @param array_val A vector for 1d array, 2d matrix and higher order matrices for higher dimensions upto 5
#' @param array_type The data type of values present in the array
#' @details The variables declared in.

new_param <- function(model, par_name, par_type, single_par_val, enum_val ,
                      set_val, set_type,  array_val, array_type){
  if(par_type == "int" || par_type == "float" || par_type == "bool"){
    # init_str contains the string pattern to be written to the mzm file
    init_str = paste0(par_type,": ", par_name, "=", single_par_val, ";\n")
    write(init_str, model, append = TRUE)
  }else if(par_type == "enum"){
    init_str = paste0(par_type,": ", par_name, "= {", paste(as.character(enum_val), collapse=", ") , "} ;\n")
    write(init_str, model, append = TRUE)
  }else if(par_type == "set"){
    init_str = paste0(par_type,": ", par_name, "=", set_val[1],".." ,set_val[2], ";\n")
    write(init_str, model, append = TRUE)
  }else if (par_type == "array"){
    if(is.null(dim(array_val))){
      
      if(length(array_val) > 0){
        init_str = paste0(par_type,"of ", array_type,": ",par_name ,"=","{", 
                          paste(as.character(array_val), collapse=", "),"} ;\n")
        write(init_str, model, append = TRUE)
      }else{
        stop("Please provide a valid array value")
      }
    }else if(length(dim(array_val))>1){
      arraynd = paste0("array",length(dim(array_val)),"d")
      init_str = paste0(par_type, "of ", array_type, ": ", par_name, "=", arraynd, "(", "1..", dim(array_val)[1],
                        ", 1..", dim(array_val)[2], ", ", paste(as.character(as.vector(unlist(array_val))),
                                                                collapse=", "),") ;\n")
      write(init_str, model, append = TRUE)
    }
  }

}
