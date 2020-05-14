#' Specify the solver options
#' @export
#' @param solver name of the solver to be used by MiniZinc "Gecode" by default.
#' @param all_solutions bool to specify if all the solutions should be printed or not. FALSE by default.
#' @param statistics bool to specify if the performance statistics should be printed or not. FALSE by default.
#' @param keep_files bool to specify if the flattened model should be kept or not. FALSE by default.
#' @param threads integer specifying the number of threads to use. 1 by default.
solver_options <- function(solver = "Gecode", all_solutions = FALSE, statistics = FALSE, keep_files = FALSE,
                           threads = 1){
  str_options = paste0("--solver ", solver," ")
  if(all_solutions){ str_options <- paste0(str_options, "-a ") }
  if(statistics) {str_options <- paste0(str_options, "-s ")}
  if(keep_files){str_options <- paste0(str_options, "-k ")}
  if(threads > 1){str_options <- paste0(str_options, "-p ", threads," ")}
  return(str_options)
}