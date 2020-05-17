#' @title get the minizinc results.
#' 
#' @description 
#' This class will get the MiniZinc results by creating the mzn file.
#' @import R6
#' @import checkmate

#' @export


results <- R6Class("results",
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
                   initialize = function(model, result_type){
                   assert_r6(model, "model")
                   self$model = model
                   
                   self$mzn = tempfile(fileext = ".mzn")
                   write_mzn(self$model, self$mzn)
                   
                   assert_choice(result_type, .globals$result_types)
                   
                   command = solver_options(self$mzn)
                   if(test_choice(result_type,"string")){
                     if(test_choice(Sys.info()["sysname"], "Linux")){
                       self$result = system(command, intern = TRUE)
                     }else if(test_choice(Sys.info()["sysname"], "Windows")){
                       self$result = system2(command, stdout = TRUE)
                     }
                   }else{
                     parsed_obj = result_R6$new(self$result)
                     self$result = parsed_obj
                   }
  
                  }))