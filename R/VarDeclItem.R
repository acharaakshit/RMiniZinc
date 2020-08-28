#' @title The variable declaration item
#' @description Declaration items in the model
#' @export
VarDeclItem = R6Class("VarDeclItem",
                      inherit = Item,
                      public = list(
                        #' @description constructor
                        #' @param decl the declaration expression object
                        #' @param mzn_str string representation of variable declaration item
                        initialize =  function(decl = NULL, mzn_str = NULL){
                          if(testCharacter(mzn_str)){
                            assertNull(decl)
                            parsedR6 = suppressWarnings(mzn_parse(model_string = mzn_str))
                            if(!testR6(parsedR6, "Model") && 
                               parsedR6$nitems() != 1 &&
                               !testR6(parsedR6$item_i(1), "VarDeclItem")){
                              stop("pass only single variable declaration")
                            }
                            vitem = parsedR6$item_i(1)
                            private$.decl = vitem$getDecl()
                          }else{
                            assertR6(decl, "VarDecl")
                            private$.decl = decl 
                          }
                        },
                        #' @description get the variable declaration
                        getDecl = function(){
                          return(private$.decl)
                        },
                        #' @description set the variable declaration
                        #' @param e var decl expression
                        setDecl = function(e){
                          assertR6(e, "VarDecl")
                          private$.decl = e
                        },
                        #' @description get the identifier object for the variable 
                        id =  function(){
                          return(private$.decl$id())
                        },
                        #' @description set the variable decaration 
                        #' @description convert the declaration to String
                        c_str = function(){
                          return(sprintf("%s;\n", private$.decl$c_str()))
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
                      ))