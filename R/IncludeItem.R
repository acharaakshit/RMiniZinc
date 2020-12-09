#' @title Include Items
#' @description 
#' Include external mzn files
#' in your model.
#' @export
IncludeItem = R6Class("IncludeItem",
                      inherit = Item,
                      public = list(
                        #' @description constructor
                        #' @param name name of the file to include
                        #' @param mzn_str string representation of Include Item
                        initialize = function(name = NULL, mzn_str = NULL){
                          if(testCharacter(mzn_str)){
                            assertNull(name)
                            parsedR6 = suppressWarnings(mzn_parse(model_string = mzn_str))
                            if(!testR6(parsedR6, "Model") &&
                               parsedR6$nitems() != 1 &&
                               !testR6(parsedR6$getItem(1), "IncludeItem")){
                              stop("only single include item should be provided")
                            }
                            i_item = parsedR6$getItem(1)  
                            private$.id = i_item$getmznName()
                          }else{
                            assertCharacter(name)
                            if(substr(name, nchar(name)-3, nchar(name)) != ".mzn"){
                              stop("name should be an mzn file")
                            }
                            private$.id = name 
                          }
                        },
                        #' get file name
                        getmznName = function(){
                          return(private$.id)
                        },
                        #' set the file name
                        #' @param name name of file
                        setmznName = function(name){
                          if(substr(name, nchar(name)-3, nchar(name)) != ".mzn"){
                            stop("name should be an mzn file")
                          }
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
                          helperDeleteItem("IncludeItem")
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