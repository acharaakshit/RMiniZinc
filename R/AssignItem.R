#' @title Assignment Items
#' @description 
#' Assign values to variables in MiniZinc
#' by creating an assignment item.
#' @export
AssignItem = R6Class("AssignItem",
                     inherit = Item,
                     public = list(
                       #' @description constructor
                       #' @param decl declaration associated with assignment.
                       #' @param value expression to be assigned.
                       initialize = function(decl, value){
                           assertR6(decl, "VarDecl")
                           assertNull(decl$value())
                           assertR6(value, "Expression")
                           private$.decl = decl
                           private$.e = value
                       },
                       #' @description  get the name of assigned variable
                       id = function(){
                         return(private$.decl$id()$getId())
                       },
                       #' @description get the value
                       getValue = function(){
                         return(private$.e)
                       },
                       #' @description set the value
                       #' @param  val value/expression to be set
                       setValue = function(val){
                         assertR6(val, "Expression")
                         private$.e = val
                       },
                       #' @description get the associated declaration
                       getDecl = function(){
                         return(private$.decl)
                       },
                       #' @description set the associated declaration
                       #' @param decl declaration to be set
                       setDecl = function(decl){
                         assertR6(decl, "VarDecl")
                         private$.decl = decl
                       },
                       #' @description get the MiniZinc representation
                       c_str = function(){
                         return(sprintf("%s = %s;\n", private$.decl$id()$getId(), private$.e$c_str()))
                       },
                       #' @description delete flag for internal use
                       getDeleteFlag = function(){
                         return(private$.delete_flag)
                       },
                       #' @description delete the assignment item
                       delete = function(){
                         private$.delete_flag = TRUE
                         helperDeleteItem("AssignItem")
                       }
                     ),
                     private = list(
                       #' @field .decl
                       #' associated declaration
                       .decl = NULL,
                       #' @field .e
                       #' value to be assigned
                       .e = NULL,
                       #' @field .delete_flag
                       #' used to delete items
                       .delete_flag = FALSE
                     ))