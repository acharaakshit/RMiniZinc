#' @title parsetoR6 class
#' 
#' @description 
#' class for parsing MiniZinc output strings and storing them as R6 objects. 
#' 
#' @import R6
#' @import checkmate
#' @import rlist
#' 
#' @export


parsetoR6 <- R6Class("parsetoR6",
                     public = list(
                       #' @field optimal_solution
                       #' stores the optimal solution. NULL if solution not found
                       optimal_solution = NULL,
                       #' @field best_solution
                       #' stores the optimal solution. NULL if solution not found
                       best_solution = NULL,
                       #' @field solutions
                       #' stores all the possible solutions.
                       solutions = NULL,
                       #' @description
                       #' initialize the class objects
                       #' @param mzn_result the result (string output) of MiniZinc model
                       initialize = function(mzn_result){
                         
                         if(test_choice(length(mzn_result),1)){
                          if(test_choice(mzn_result,"----------")){
                            stop("Empty model was given to MiniZinc")
                          } else if(test_choice(mzn_result,"=====UNSATISFIABLE=====")) {
                            self$solutions = "No Solution"
                            stop("There is no solution to this model")
                          }
                         }
                         
                         # find all the solutions
                         track_solutions = which(mzn_result == "----------")
                         solutions = list()
                         start = 0
                         for(n in track_solutions){
                           index1 = start
                           index2 = n
                           sol = mzn_result[(index1+1):(index2-1)]
                           sol = gsub(";","", sol)
                           sol = str2expression(sol)
                           sol_names = sapply(sol, function(x) x[[2]])
                           sol_values = as.list(sapply(sol, eval))
                           names(sol_values) = sol_names
                           solutions = list.append(solutions, sol_values)
                           start = n
                         }
                         
                         self$solutions = solutions
                         
                         # best solution is the last solution found
                         self$best_solution = solutions[[length(solutions)]]
                         
                         # get the optimal solution if there is any
                         if(test_choice(mzn_result[[length(mzn_result)]], "==========")){
                            self$optimal_solution = self$best_solution
                          }
                         
                       }
                     ))
