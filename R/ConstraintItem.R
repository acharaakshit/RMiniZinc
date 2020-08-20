#' @title Constraint Class
#' @description
#' Describe Minizinc constraints on decision variables.
#' @export
ConstraintItem = R6Class("ConstraintItem",
                         inherit = Item,
                         public = list(
                           #' @description
                           #' Creates a new instance of Constraint class.
                           #' @param e The expression for the constraint (used if e is NULL)
                           #' @param mzn_str string representation of Constraint item
                           initialize = function(e = NULL, mzn_str = NULL){
                             if(testCharacter(mzn_str)){
                               assertNull(e)
                               parsedList = suppressWarnings(mzn_parse(modelString = mzn_str))
                               if(!testTRUE(length(parsedList) == 2 &&
                                           length(parsedList$CONSTRAINTS) == 1 &&
                                           names(parsedList$CONSTRAINTS) == "CONSTRAINT1")){
                                 stop("pass only single constraint")
                               }
                               private$.e = initExpression(parsedList$CONSTRAINTS$CONSTRAINT1$DETAILS)
                             }else{
                               assertR6(e, "Expression")
                               private$.e = e 
                             }
                           },
                           #' @description get the constraint expression
                           getExp = function(){
                             return(private$.e)
                           },
                           #' @description set the constraint expression
                           #' @param e expression
                           setExp = function(e){
                             assertR6(e, "Expression")
                             private$.e = e
                           },
                           #' @description serialize to MiniZinc syntax
                           c_str = function(){
                             return(sprintf("constraint %s;\n", private$.e$c_str()))
                           },
                           #' @description delete flag for internal use
                           getDeleteFlag = function(){
                             return(private$.delete_flag)
                           },
                           #' @description delete the constraint item
                           delete = function(){
                             private$.delete_flag = TRUE
                             pf = parent.frame()
                             items = sapply(ls(pf), function(i) {
                               class(get(i, envir = pf))[1] == "ConstraintItem"
                             })
                             this = ls(pf)[items][sapply(mget(ls(pf)[items], envir = pf),
                                                         function(x) x$getDeleteFlag())]
                             rm(list = this, envir = pf)
                             message("ConstraintItem object deleted!")
                           }
                         ),
                         private = list(
                           #' @field .e
                           #' the constraint expression
                           .e = NULL,
                           #' @field .delete_flag
                           #' used to delete items
                           .delete_flag = FALSE
                         ))