#' @title The variable declaration item
#' 
#' @description Declaration items in the model
#' 
#' @import R6
#' @import checkmate
#' 
#' @export
VarDeclItem = R6Class("VarDeclItem",
                      inherit = Item,
                      public = list(
                        #' @description constructor
                        #' @param decl the declaration expression object
                        initialize =  function(decl){
                          assertR6(decl, "VarDecl")
                          private$.decl = decl
                        },
                        #' @description get the declaration expression object
                        e = function(){
                          return(private$.decl)
                        },
                        #' @description convert the declaration to String
                        c_str = function(){
                          return(sprintf("%s;", private$.decl$c_str()))
                        },
                        #' @description delete flag for internal use
                        getDeleteFlag = function(){
                          return(private$.delete_flag)
                        },
                        #' @description delete the variable item
                        delete = function(){
                          private$.delete_flag = TRUE
                          pf = parent.frame()
                          items = sapply(ls(pf), function(i) {
                            class(get(i, envir = pf))[1] == "VarDeclItem"
                          })
                          this = ls(pf)[items][sapply(mget(ls(pf)[items], envir = pf),
                                                       function(x) x$getDeleteFlag())]
                          rm(list = this, envir = pf)
                          gc()
                          message("VarDeclItem object deleted!")
                        }
                      ),
                      private = list(
                        #' @field .decl
                        #' the declaration expression
                        .decl = NULL,
                        #' @field .delete_flag
                        #' used to delete items
                        .delete_flag = FALSE
                      )
                    )
