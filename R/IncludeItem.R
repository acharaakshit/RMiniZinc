#' @title Include Items
#' @description include files in MiniZinc
#' @import R6
#' @import checkmate
#' @export
IncludeItem = R6Class("IncludeItem",
                      inherit = Item,
                      public = list(
                        #' @description constructor
                        #' @param name name of the file to include
                        initialize = function(name){
                          assertCharacter(name)
                          private$.id = name
                        },
                        #' get file name
                        getmznName = function(){
                          return(private$.id)
                        },
                        #' set the file name
                        #' @param name name of file
                        setmznName = function(name){
                          private$.id = name
                        },
                        #' @description get the MiniZinc representation
                        c_str = function(){
                          return(sprintf("include %s;\n", shQuote(private$.id, "cmd")))
                        },
                        #' @description delete flag for internal use
                        getDeleteFlag = function(){
                          return(private$.delete_flag)
                        },
                        #' @description delete the include item
                        delete = function(){
                          private$.delete_flag = TRUE
                          pf = parent.frame()
                          items = sapply(ls(pf), function(i) {
                            class(get(i, envir = pf))[1] == "IncludeItem"
                          })
                          this = ls(pf)[items][sapply(mget(ls(pf)[items], envir = pf),
                                                      function(x) x$getDeleteFlag())]
                          rm(list = this, envir = pf)
                          message("IncludeItem object deleted!")
                        }
                      ),
                      private = list(
                        #' @field .id
                        #' name of mzn file
                        .id = NULL,
                        #' @field .delete_flag
                        #' used to delete items
                        .delete_flag = FALSE
                      ))