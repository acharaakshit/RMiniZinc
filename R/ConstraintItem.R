#' @title Constraint Items
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
                               parsedR6 = suppressWarnings(mzn_parse(model_string = mzn_str))
                               if(!testR6(parsedR6, "Model") &&
                                  parsedR6$nitems() != 1 &&
                                  !testR6(parsedR6$getItem(1), "ConstraintItem")){
                                 stop("pass only single constraint")
                               }
                               citem = parsedR6$getItem(1)
                               private$.e = citem$getExp()
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
                             helperDeleteItem("ConstraintItem")
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