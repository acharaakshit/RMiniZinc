#' @title get the minizinc results.
#' 
#' @description 
#' This class will get the MiniZinc results by creating the mzn file.
#' @import R6
#' @import checkmate

#' @export


results = R6Class("results",
                   public = list(
                   #' @field mzn
                   #' temporary file for writing a model
                   mzn = NULL,   
                   #' @field model
                   #' MiniZinc \code{\link{model}}   
                   model = NULL,
                   #' @field result_type
                   #' specify whether output is required as string or R6
                   result_type = NULL,
                   #' @field result
                   #' the result of the model
                   result = NULL,
                   #' @description 
                   #' obtain the results from the model
                   #' @param model object of the model class
                   #' @param result_type string or R6 MiniZinc output
                   #' @param all_solutions boolean to specify is all solutions are needed. FALSE by default.
                   #' @param statistics boolean to specify if statistics are required. FALSE by default.
                   #' @param threads number of threads to be used by MiniZinc. 1 by default.
                   #' @param keep_files bool to specify if the flattened model should be kept or not. FALSE by default.
                   #' @param timeout time in milis (integer) specifying the timeout limit. NA by default.
                   initialize = function(model, result_type, all_solutions = FALSE, 
                                         statistics = FALSE, threads = 1, keep_files = FALSE,
                                         timeout = NA){
                   assert_r6(model, "model")
                   self$model = model
                   
                   self$mzn = tempfile(fileext = ".mzn")
                   write_mzn(self$model, self$mzn)
                   
                   assert_choice(result_type, .globals$result_types)
                   
                   command = solver_options(self$mzn, all_solutions = all_solutions, 
                                            statistics = statistics, threads = threads)
                   
                   if(test_choice(Sys.info()["sysname"], "Linux") || test_choice(Sys.info()["sysname"], "Darwin")){
                     result = system(command, intern = TRUE)
                   }else if(test_choice(Sys.info()["sysname"], "Windows")){
                     result = system2(command, stdout = TRUE)
                   }
                   
                   assert(test_character(result), test_true(length(result)>0), combine = "and")
                   
                  
                   
                   assert_choice(result_type, c("string", "R6"))
                   
                   if(test_choice(result_type,"string")){
                     self$result = result
                   }else{
                     parsed_obj = parsetoR6$new(result)
                     self$result = parsed_obj
                   }
  
                  }))