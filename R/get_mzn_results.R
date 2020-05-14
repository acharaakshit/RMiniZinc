#' get the minizinc results from the command line
#' @export
#' @param model the path of the temporary model file.
#' @param str_options string returned by solver_options()
#' @param as_string bool to specify if output is required as a string.
#' @param as_R6 bool to specify if output is required as an R6 class object.
#' @param timeout time in milis (integer) specifying the timeout limit. NA by default.
get_mzn_results <- function(model, str_options, as_string = TRUE, as_R6 = FALSE,
                            timeout = NA){
  timeout_str  = " "
  if(!is.na(timeout)) { timeout_str = paste0(' --fzn-flags /"-time ', timeout   , '/" ')}
  if(Sys.info()["sysname"] == "Linux"){
    output_str <- as.character(system(paste0("minizinc ", str_options, model, timeout_str), intern = TRUE))
  }else if(Sys.info()["sysname"] == "Windows"){
    output_str <- system2(paste0("minizinc ", str_options, model, timeout_str), stdout = TRUE)
  }
  
  if(as_string){ return(output_str)}
  else if(as_R6) {  return(parse_R6(output_str))}
  else{stop("output can only be returned as a string, json object or an R6 class object")}
  
}