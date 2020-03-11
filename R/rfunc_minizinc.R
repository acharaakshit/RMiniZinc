#' create the variables
#' @export
#' @param  single_var_names list of names of basic/single variables/parameters(not collection)
#' @param single_var_types  list of data types of basic/single variables.Possible types are "int"
#' "float", "string" and "bool".
#' @param collection_var_names list of names of collection variables/parameters(array or set)
#' @param collection_types list of types of collection.Possible types are "array" or
#' "set".
#' @param collection_var_types list of data types of collection variables/parameters.
#' @param array_var_ranges list of array ranges in order of  the array variables used.typical element in the list
#' should look like c(lower_limit,upper_limit), for eg. c(2,10) for 2D array and single numbers for eg 7 for
#' 1D array .
#' @param append_bool bool to tell if it is required to open the file in append mode. FALSE by default
#' @param data_file_bool bool to determine if the user wants to initialize the parameters using a data file
#' @param single_var_values list of values of the single variables/parameters.
#' @param collection_var_values list of values of the collection variables.
#' @param  path the path of the model.mzn file

create_vars <- function(single_var_names,single_var_types, collection_var_names=c(),collection_types=c(),
                        collection_var_types=c(), array_var_ranges=c(), append_bool = FALSE,data_file_bool = TRUE,
                        single_var_values=c(), collection_var_values=c(),path){
  
  sink(path, type = "output", append = append_bool)
  if(data_file_bool == FALSE){
    array_count <- 1
    for (x in seq(1,length(single_var_names),1)) {
      writeLines(paste0(single_var_types[x],": ",single_var_names[x],"=",
                        as.character(single_var_values[x]),";","\n"))
    }
    if(length(collection_var_types)>0){
      for (x in seq(1, length(collection_var_types),1)) {
        if(collection_types[x] == "set"){
          writeLines(paste0("set of ",collection_var_types[x],": ", collection_var_names[x],
                            "=",collection_var_values[x],";"))
        }else if(collection_types[x] == "array" & length(array_var_ranges[[x]]) == 2){
          writeLines(paste0("array[",array_var_ranges[[x]][1],",",array_var_ranges[[x]][2],
                            "] of ", collection_var_types[x],": ", collection_var_names[x],
                            "=",collection_var_values[x],";"))
        }else if(collection_types[x] == "array" & length(array_var_ranges[[x]]) == 1) {
          writeLines(paste0("array[",array_var_ranges[[x]][1],"] of ",
                            collection_var_types[x],": ", collection_var_names[x],
                            "=",collection_var_values[x],";"))
        }
      }
    }
  } else if(data_file_bool == TRUE){
    array_count <- 1
    for (x in seq(1,length(single_var_names),1)) {
      writeLines(paste0(single_var_types[x],": ",single_var_names[x],"; \n"))
    }
    if(length(collection_var_types)>0){
      for (x in seq(1, length(collection_var_types),1)) {
        if(collection_types == "set"){
          writeLines(paste0("set of ",collection_var_types[x],": ", collection_var_names[x],";"))
        }else if(collection_types == "array" & length(array_var_ranges[[x]]) == 2){
          writeLines(paste0("array[",array_var_ranges[[x]][1],",",array_var_ranges[[x]][2],
                            "] of ", collection_var_types[x],": ", collection_var_names[x],";"))
        }else if(collection_types == "array" & length(array_var_ranges[[x]]) == 1) {
          writeLines(paste0("array[",array_var_ranges[[x]][1],"] of ",
                            collection_var_types[x],": ", collection_var_names[x],";"))
        }
      }
    }
  }
  sink()
}

#' add solvers for the model (uses can be extended later )
#' @export
#' @param include_name string containing the name of the solver to be used.
#' @param append_bool bool to tell if it is required to open the file in append mode. TRUE by default
#' currently only a few solvers like Geocode can be used, the code will be modified later

add_solvers <- function(include_name,append_bool = TRUE){
  sink("/home/akshit/model.mzn", type = "output", append = append_bool)
  writeLines(paste0("include<",include_name,">;\n"))
  sink()
}

#' create the decision variables
#' @export
#' @param dec_var_names list of names of single decision variables
#' @param dec_var_ranges list of ranges of the single decision variables. Typical range should look like c(2,10).
#' @param dec_var_collection_names list of names of collection decision variables(array or set)
#' @param dec_collection_types list of types of collection.Possible types are "array" or "set".
#' @param dec_var_collection_types list of data types of collection of decision variables.
#' @param array_var_ranges list of array ranges in order of  the array variables used.typical element in the list
#' should look like c(lower_limit,upper_limit), for eg. c(2,10) for 2D array and single numbers for eg 7 for
#' 1D array .
#' @param append_bool bool to tell if it is required to open the file in append mode. TRUE by default
#' @param path the path for model mzn

create_decision_vars <- function(single_dec_var_names, single_dec_var_ranges,dec_var_collection_names=c(),
                                 dec_collection_types=c(), dec_var_collection_types=c(), dec_var_array_ranges=c(),
                                 append_bool = TRUE, path){
  
  sink(path, type = "output", append = append_bool)
  
  for (x in seq(1,length(single_dec_var_names),1)) {
    writeLines(paste0("var ",single_dec_var_ranges[[x]][1],"..",single_dec_var_ranges[[x]][2],
                      ": ",single_dec_var_names[x],";","\n"))
  }
  if(length(dec_var_collection_types)>0){
    for (x  in seq(1, length(dec_var_collection_names),1)) {
      if(dec_collection_types[x] == "set"){
        writeLines(paste0("set of ",dec_var_collection_types[x],": ", dec_var_collection_names[x],";"))
      }else if(dec_collection_types[x] == "array" & length(dec_var_array_ranges[[x]]) == 2){
        writeLines(paste0("array[",dec_var_array_ranges[[x]][1],",",dec_var_array_ranges[[x]][2],
                          "] of var ", dec_var_collection_types,": ", dec_var_collection_names,";"))
      }else if(dec_collection_types[x] == "array" & length(dec_var_array_ranges[[x]]) == 1) {
        writeLines(paste0("array[",dec_var_array_ranges[[x]][1],"] of var ",
                          dec_var_collection_types[x],": ", dec_var_collection_names[x],";"))
      }else{
        stop("enter correct collection type or ranges")
      }
    }
  }
  sink()
  
}

#' setup the constraints of the model. structure of output should look like constraint 〈Boolean expression〉;
#' @export
#' @param constraint_list list of the constraints of the model
#' @param append_bool bool to tell if it is required to open the file in append mode. TRUE by default
#' @param  path the oath of the model_mzn file

setup_constraints <- function(constraint_list, append_bool = TRUE, path){
  sink(path, type = "output", append = append_bool)
  for (x in seq(1,length(constraint_list),1)) {
    writeLines(paste0("constraint ",as.character(constraint_list[x]),";","\n"))
  }
  sink()
}

#' mention the type of problem
#' @export
#' @param type_of_problem the type of optimization problem. choose s for satisfaction problem,
#'  min for minimization problem and max for maximization
#'  @param arithmetic_expression string containing arithmetic expression for maximization and minimization
#'  problems. Empty by default.
#' @param append_bool bool to tell if it is required to open the file in append mode. TRUE by default
#' @param path the path of model mzn

setup_solvers <- function(type_of_problem, arithmetic_expression = " ",append_bool = TRUE, path){
  
  sink(path, type = "output", append = append_bool)
  
  switch(type_of_problem,
         s={
           writeLines("solve satisfy; \n")
         },
         min={
           writeLines(paste0("solve minimize ",arithmetic_expression,";","\n"))
         },
         max={
           writeLines(paste0("solve maximize ",arithmetic_expression,";","\n"))
         },
         {
           print('please enter correct option')
         }
  )
  sink()
  
}

#' setup the output scheme
#' @export
#' @param output_vars the list of output variables, i.e the variables you want to print
#' @param append_bool bool to tell if it is required to open the file in append mode. TRUE by default
#' @param default_output_bool bool to determine if you want to output all the decision variables.TRUE by default.
#' @param path the path of model mzn

setup_output <- function(output_vars=c(), append_bool = TRUE, default_output_bool = TRUE, path){
  if(!default_output_bool){
    sink(path, type = "output", append = append_bool)
    for (x in seq(1,length(output_vars),1)) {
      str1 <- sprintf('output [ "%s="',output_vars[x])
      writeLines(paste0(str1," , show(",output_vars[x],'), "\t"] ;' , "\n"))
    }
    sink()
  }
}


#' create data file
#' @export
#' @param input_var_names list of names of the input variables
#' @param input_var_values list of values of input variables
#' @param path the path of data dzn

create_data_dzn <- function(input_var_names,input_var_values, path){
  sink(path,type = "output")
  for (x in seq(1,length(input_var_names),1)) {
    writeLines(paste0(input_var_names[x], " = ", input_var_values, ";\n"))
  }
  sink()
}


#' get the results/solution
#' @export
#' @param path_to_model_mzn The path of your model (for eg. model.mzn or model.fzn in case its already compiled) file
#' @param path_to_data_dzn The path of data (for eg. data.dzn) file
#' @param task The string named task ("compile","standardize" or "solve") that you want to perform.solve by default
#' @param solver The name of solver you want to use. Gecode by default
#' @param path_to_model_fzn The path to fzn file if model is already compiled. Empty by default
#' @param path_to_model_ozn The path to ozn file if there is FlatZinc output. Empty by default
#' @param FlatZinc_bool a bool to determine if there is a FlatZinc output file. FALSE by default
#' @param data_file_bool  a bool to determine if data file was used as input for parameters. TRUE by default

get_results <- function(path_to_model_mzn, path_to_data_dzn,task = "solve", solver = "Gecode",
                        path_to_model_fzn=" ", path_to_model_ozn=" ", FlatZinc_bool = FALSE,
                        data_file_bool = TRUE){
  if(data_file_bool){
    if(task == "compile"){
      return(system(paste0("minizinc ",task," ", "--solver ", solver," " ,path_to_model_mzn,
                           " ", path_to_data_dzn ), intern = TRUE))
    }else if(task == "solve" & FlatZinc_bool == FALSE){
      return(system(paste0("minizinc --solver ", solver," " ,path_to_model_mzn,
                           " ", path_to_data_dzn ), intern = TRUE))
    }else if(task == "standardize"){
      return(system(paste0("minizinc --solver ",solver," ",path_to_model_mzn)))
    }else{
      return(system(paste0("minizinc --solver ",solver," ",path_to_model_fzn,
                           " |  minizinc --ozn-file ",path_to_model_ozn)))
    }
  }else{
    if(task == "compile"){
      return(system(paste0("minizinc ",task," ", "--solver ", solver," " ,path_to_model_mzn,
                           " " ), intern = TRUE))
    }else if(task == "solve" & FlatZinc_bool == FALSE){
      return(system(paste0("minizinc --solver ", solver," " ,path_to_model_mzn,
                           " " ), intern = TRUE))
    }else if(task == "standardize"){
      return(system(paste0("minizinc --solver ",solver," ",path_to_model_mzn)))
    }else{
      return(system(paste0("minizinc --solver ",solver," ",path_to_model_fzn,
                           " |  minizinc --ozn-file ",path_to_model_ozn)))
    }
  }
}

#'A function to setup the checker file
#' @export
#' @param dec_vars the decision variables to be written to the checker file.
#' @param  constraint_list list of constraints of the model.
#' @param path th epath of model_mzc_mzn file

setup_checker_model_input <- function(dec_var_names, dec_var_types, constraint_list, path){
  sink(path, type = "output")
  for (x in seq(1,length(dec_var_names),1)) {
    writeLines(paste0(dec_var_types[x],": ",dec_var_names[x],"; \n"))
  }
  writeLines("output [\n if ")
  for (x in seq(1, length(constraint_list)-1, 1)) {
    writeLines(paste0(constraint_list[x], " /\\ " ))
  }
  writeLines(paste0(constraint_list[x],'\n then "CORRECT" \n else "INCORRECT" \n endif ];   '))
  
}

#' Function to check the solution
#' @export
#' @param path_of_model_mzn the path of the model.mzn file
#' @param path_of_data_dzn the path of the data.dzn file
#' @param path_of_model_mzc_mzn the path of model.mzc.mzn file
#' @param data_file_bool the bool to determine if you want to use data file to initialize the parameters
#' @param compile_bool the bool to determine if you wish to compiler the checker file
#' @param path_of_model_mzc the path of the compiled checker file model.mzc
#' @param compiled the bool to determine if the checker file is alrady compiled

check_results <- function(path_of_model_mzn, path_of_data_dzn, path_of_model_mzc_mzn,
                          data_file_bool = FALSE,compile_bool = FALSE,path_of_model_mzc=" ",
                          compiled = FALSE){
  if(data_file_bool == FALSE){
    if(compile_bool == FALSE & compiled == FALSE){
      return(system(paste0("minizinc " ,path_of_model_mzn,
                           " ", path_of_model_mzc_mzn ), intern = TRUE))
    }else if(compile_bool == FALSE & compiled == TRUE){
      return(system(paste0("minizinc " ,path_of_model_mzn,
                           " ", path_of_model_mzc ), intern = TRUE))
    }else{
      system(paste0("minizinc --compile-solution-checker ",path_of_model_mzc_mzn))
    }
  }else{
    return(system(paste0("minizinc --solver ", solver," " ,path_to_model_mzn,
                         " ",path_of_model_mzc_mzn," ", path_of_data_dzn ), intern = TRUE))
  }
}

