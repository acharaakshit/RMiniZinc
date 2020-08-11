#' @title SolveItem
#' @description specify whether the optimization problem is a satisfaction,
#' minimization or maximization problem and/or expression to maximize/minnimize
#' and/or annotation
#' @import R6
#' @import checkmate
#' @export
SolveItem = R6Class("SolveItem",
                    inherit = Item,
                           public = list(
                             #' @description create an instance of specify_problem class
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
                             #' @description get the expression
                             getExp =  function(){
                               return(private$.e)
                             },
                             #' @description set the expression
                             #' @param e expression
                             setExp =  function(e){
                               assertTRUE(private$.st == "minimize" ||
                                            private$.st == "maximize")
                               assertR6(e, "Expression")
                               private$.e = e
                             },
                             #' @description get the solve type/objective
                             getSt = function(){
                               return(private$.st)
                             },
                             #' @description set the solve type/objective
                             #' @param objective solve type
                             setSt = function(objective){
                               assert_choice(objective, .globals$objectives)
                               private$.st = objective
                             },
                             #' @description to string method
                             c_str = function(){
                               annStr = ""
                               if(!is.null(private$.ann)){
                                 annStr = private$.ann$c_str()
                               }
                               if(private$.st == "satisfy"){
                                 return(sprintf("solve %s satisfy;\n", annStr))
                               }else{
                                 return(sprintf("solve %s %s %s;\n", annStr, private$.st, private$.e$c_str()))
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
                            #' @field  .e
                            #' the expression to maximize or minimize
                            .e = NULL,
                            #' @field  .st
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
