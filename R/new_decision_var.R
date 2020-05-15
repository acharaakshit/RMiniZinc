#' create the variables.
#' @export
#' @param single_dec_var_names list of names of single decision variables
#' @param single_dec_var_ranges list of ranges of the single decision variables. Typical range should look like c(2,10).
#' @param dec_var_collection_names list of names of collection decision variables(array or set)
#' @param dec_collection_types list of types of collection.Possible types are "array" or "set".
#' @param dec_var_collection_types list of data types of collection of decision variables.
#' @param dec_var_array_ranges list of array ranges in order of  the array variables used.typical element in the list
#' should look like c(lower_limit,upper_limit), for eg. c(2,10) for 2D array and single numbers for eg 7 for
#' 1D array .
#' @param model the path for model mzn

create_decision_vars <- function(single_dec_var_names, single_dec_var_ranges,dec_var_collection_names=c(),
                                 dec_collection_types=c(), dec_var_collection_types=c(), dec_var_array_ranges=c(),
                                 model){
  
  for (x in seq(1,length(single_dec_var_names),1)) {
    write(paste0("var ",single_dec_var_ranges[[x]][1],"..",single_dec_var_ranges[[x]][2],
                      ": ",single_dec_var_names[x],";","\n"),model, append = TRUE)
  }
  if(length(dec_var_collection_types)>0){
    for (x  in seq(1, length(dec_var_collection_names),1)) {
      if(dec_collection_types[x] == "set"){
        write(paste0("set of ",dec_var_collection_types[x],": ", dec_var_collection_names[x],";"),
                      model, append = TRUE)
      }else if(dec_collection_types[x] == "array" & length(dec_var_array_ranges[[x]]) == 2){
        write(paste0("array[",dec_var_array_ranges[[x]][1],",",dec_var_array_ranges[[x]][2],
                          "] of var ", dec_var_collection_types,": ", dec_var_collection_names,";"),
                          model, append = TRUE)
      }else if(dec_collection_types[x] == "array" & length(dec_var_array_ranges[[x]]) == 1) {
        write(paste0("array[",dec_var_array_ranges[[x]][1],"] of var ",
                          dec_var_collection_types[x],": ", dec_var_collection_names[x],";"),
                          model, append = TRUE)
      }else{
        stop("enter correct collection type or ranges")
      }
    }
  }
  
  
}
