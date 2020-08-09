#' @title 
#' mention if the problem is satisfaction, minimization or maximization.
#' 
#' @description 
#' This is a class that will be used to specify whether the optimization problem is a satisfaction,
#' minimization or maximization problem
#' 
#' @import R6
#' @import checkmate
#' 
#' 
#' @export

SolveItem = R6Class("SolveType",
                    inherit = Item,
                           public = list(
                             #' @description 
                             #' create an instance of specify_problem class
                             #' @param solve_type satisfaction, minimization or maximization
                             #' @param e expression to minimize or maximize
                             #' @param ann annotation
                             initialize = function(solve_type, e = NULL, ann = NULL){
                                
                                  assert_choice(solve_type, .globals$objectives)
                                  private$.st = solve_type
                                  assertTRUE(testR6(ann, "Annotation") || testNull(ann))
                                  private$.ann = ann
                                  if(test_choice(solve_type, "satisfy")){
                                    assert_null(e)
                                  }else{
                                    assertR6(e, "Expression")
                                    private$.e = e
                                  }
                                  
                                },
                             #' @description return the expression
                             e =  function(){
                               return(private$.e)
                             },
                             st = function(){
                               return(private$.st)
                             },
                             #' @description to string method
                             c_str = function(){
                               annStr = ""
                               if(!is.null(private$.ann)){
                                 annStr = private$.ann$c_str()
                               }
                               if(private$.st == "satisfy"){
                                 return(sprintf("solve %s satisfy;", annStr))
                               }else{
                                 return(sprintf("solve %s %s %s;", annStr, private$.st, private$.e$c_str()))
                               }
                             },
                             #' @description delete flag for internal use
                             getDeleteFlag = function(){
                               return(private$.delete_flag)
                             },
                             #' @description delete the solve item
                             delete = function(){
                               private$.delete_flag = TRUE
                               pf = parent.frame()
                               items = sapply(ls(pf), function(i) {
                                 class(get(i, envir = pf))[1] == "SolveItem"
                               })
                               this = ls(pf)[items][sapply(mget(ls(pf)[items], envir = pf),
                                                            function(x) x$getDeleteFlag())]
                               rm(list = this, envir = pf)
                               message("SolveItem object deleted!")
                             }
                             ),
                          private = list(
                            #' @description .e
                            #' the expression to maximize or minimize
                            .e = NULL,
                            #' @description .st
                            #' the solve type
                            .st = NULL,
                            #' @field .ann
                            #' annotation of the solve type
                            .ann = NULL,
                            #' @field .delete_flag
                            #' used to delete items
                            .delete_flag = FALSE
                          )
                    )
