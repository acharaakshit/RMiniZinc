#' @title Assignment Items
#' @description assignments in MiniZinc
#' @export
AssignItem = R6Class("AssignItem",
                     inherit = Item,
                     public = list(
                       #' @description constructor
                       #' @param decl declaration associated with assignment.
                       #' @param value expression to be assigned.
                       #' @param mzn_str string representation of Assignment Item
                       initialize = function(decl, value = NULL, mzn_str = NULL){
                         if(testCharacter(mzn_str)){
                           parsedList = suppressWarnings(mzn_parse(modelString = mzn_str))
                           if(!testTRUE(length(parsedList) == 2) &&
                              names(parsedList$ASSIGNMENTS) == "ASSIGNMENT1"){
                             stop("Supply only a single assignment item in mzn_str")
                           }
                           private$.e = initExpression(parsedList$ASSIGNMENTS$ASSIGNMENT1$VALUE)
                           assertR6(decl, "VarDecl")
                           assertNull(decl$value())
                           if(!testTRUE(parsedList$ASSIGNMENTS$ASSIGNMENT1$NAME == decl$id()$getId())){
                             stop("the names of supplied declaration and assignment don't match")
                           }
                           private$.decl = decl
                         }else{
                           assertR6(decl, "VarDecl")
                           assertNull(decl$value())
                           assertR6(value, "Expression")
                           private$.decl = decl
                           private$.e = value 
                         }
                       },
                       #' @description  get the name of assigned variable
                       id = function(){
                         return(private$.decl$id())
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