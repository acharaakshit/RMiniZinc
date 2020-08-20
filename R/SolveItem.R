#' @title SolveItem
#' @description specify whether the optimization problem is a satisfaction,
#' minimization or maximization problem and/or expression to maximize/minnimize
#' and/or annotation
#' @export
SolveItem = R6Class("SolveItem",
                    inherit = Item,
                           public = list(
                             #' @description create an instance of specify_problem class
                             #' @param solve_type satisfaction, minimization or maximization
                             #' @param e expression to minimize or maximize
                             #' @param ann annotation
                             #' @param mzn_str string representation of Solve Item
                             initialize = function(solve_type = NULL, e = NULL, ann = NULL, mzn_str = NULL){
                              if(testCharacter(mzn_str)){
                                assertTRUE(testNull(solve_type) &&
                                             testNull(e) &&
                                             testNull(ann))
                                parsedList = suppressMessages(suppressWarnings(invisible(mzn_parse(modelString = mzn_str))))
                                 if(!testTRUE(length(parsedList) == 2 &&
                                             all(names(parsedList) == c("SOLVE_TYPE", "MODEL_STRING")))){
                                  stop("provide only single solve item")  
                                 } 
                                 private$.st = parsedList$SOLVE_TYPE$OBJECTIVE
                                 if(private$.st == "SATISFY"){
                                   if(!testNull(parsedList$SOLVE_TYPE$DETAILS$EXPRESSION)){
                                     stop("satisfaction solve item should not have an expression")
                                   }
                                 }
                                 private$.e = initExpression(parsedList$SOLVE_TYPE$DETAILS$EXPRESSION)
                                 private$.ann = initExpression(parsedList$SOLVE_TYPE$DETAILS["ANNOTATION"])
                               }else{
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
                               }
                                },
                             #' @description get the expression (or NULL)
                             getExp =  function(){
                               return(private$.e)
                             },
                             #' @description get the annotation (or NULL)
                             getAnn =  function(){
                               return(private$.ann)
                             },
                             #' @description set the expression
                             #' @param e expression
                             setExp =  function(e){
                               assertTRUE(private$.st == "minimize" ||
                                            private$.st == "maximize")
                               assertR6(e, "Expression")
                               private$.e = e
                             },
                             #' @description set the annotation
                             #' @param ann annotation
                             setAnn =  function(ann){
                               assertR6(ann, "Annotation")
                               private$.ann = ann
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
