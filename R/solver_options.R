#' @title command line options for MiniZinc.
#' 
#' @description 
#' Specify the different command line options that can be used with MiniZinc.
#' 
#' @export
#' @param model the path of the temporary model.
#' @param solver name of the solver to be used by MiniZinc "Gecode" by default.
#' @param all_solutions bool to specify if all the solutions should be printed or not. FALSE by default.
#' @param statistics bool to specify if the performance statistics should be printed or not. FALSE by default.
#' @param keep_files bool to specify if the flattened model should be kept or not. FALSE by default.
#' @param threads integer specifying the number of threads to use. 1 by default.
#' @param timeout time in milis (integer) specifying the timeout limit. NA by default.

solver_options = function(model, solver = "Gecode", all_solutions = FALSE, statistics = FALSE, keep_files = FALSE,
                           threads = 1, timeout = NA){
  assert_choice(solver, .globals$solvers)
  str_options = paste0("--solver ", solver," ")
  if(all_solutions){ str_options = paste0(str_options, "-a ") }
  if(statistics) {str_options = paste0(str_options, "-s ")}
  if(keep_files){str_options = paste0(str_options, "-k ")}
  if(threads > 1){str_options = paste0(str_options, "-p ", threads," ")}
  
  timeout_str  = " "
  if(!is.na(timeout)) { timeout_str = paste0(' --fzn-flags /"-time ', timeout   , '/" ')}
  command_str = paste0("minizinc ", str_options, model, timeout_str)
  return(command_str)  
}