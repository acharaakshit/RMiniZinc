#' mention if the problem is satisfaction, minimization or maximization.
#' @export
#' @param model the path of model mzn
#' @param type_of_problem the type of optimization problem. choose s for satisfaction problem,
#'  min for minimization problem and max for maximization
#' @param arithmetic_expression string containing arithmetic expression for maximization and minimization
#'  problems. Empty by default.

specify_problem <- function(model,type_of_problem, arithmetic_expression = " "){
    switch(type_of_problem,
         s={
           write("solve satisfy; \n", model, append = TRUE)
         },
         min={
           write(paste0("solve minimize ",arithmetic_expression,";","\n"), model, append = TRUE)
         },
         max={
           write(paste0("solve maximize ",arithmetic_expression,";","\n"), model, append = TRUE)
         },
         {
           print('please enter correct option')
         }
  )
  
}