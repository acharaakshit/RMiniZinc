#' @title MiniZinc Model class
#' @description 
#' This class will take all the objects required to create a MiniZinc model.
#' @import R6
#' @import checkmate
#' @export

Model = R6Class("Model", 
                 public = list(
                 #' @description create a new instance of model class
                 #' @param items all items of the model 
                 initialize = function(items){
                   assert_list(items, "Item")
                   # check if more than one solve item is present
                   sI = 0
                   for(i in seq(1, length(items), 1)){
                     if(testR6(items[[i]], "SolveItem")){
                       sI = sI+1
                     }
                   }
                   if(sI>1){
                     stop("Only one solve Item is allowed")
                   }else if(sI == 0){
                     warning("Atleast one solve item should be present to evaluate the model")
                   }
                   private$.items = items
                 },
                 #' @description get the item using index
                 #' @param i index
                 item_i = function(i){
                   return(private$.items[[i]])
                 },
                 #' @description get the string representation of the model
                 mzn_string = function(){
                   mzn_str = ''
                   for( i in seq(1,length(private$.items),1)) {
                     mzn_str = paste0(mzn_str, private$.items[[i]]$c_str(), "\n")
                   }
                   return(mzn_str)
                 },
                 #' @description delete item i from the model
                 #' @param i index of the item
                 delete = function(i){
                   private$.items[[-i]]
                 }
                 ),
                private = list(
                  #' @field parameters
                  #' vector of variable declarations of the model
                 .items = NULL
                )
                )

#' @title Item class (Abstract)
#' @description abstract class for all items of MiniZinc
#' @import R6
#' @import checkmate
#' @export
Item = R6Class("Item",
               public = list(
                 #' @description constructor
                 initialize = function(){
                   stop(paste(RSmisc::getR6Class(self), "can't be initialized."))  
                 }
               ))

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

#' @title Assignment Items
#' @description assignments in MiniZinc
#' @import R6
#' @import checkmate
#' @export
AssignItem = R6Class("AssignItem",
                     inherit = Item,
                     public = list(
                       #' @description constructor
                       #' @param decl declaration associated with assignment
                       #' @param value expression to be assigned
                       initialize = function(decl, value){
                         assertR6(decl, "VarDecl")
                         assertNull(decl$e())
                         assertR6(value, "Expression")
                         private$.decl = decl
                         private$.e = value
                       },
                       #' @description  get the name of assigned variable
                       id = function(){
                         return(private$.decl$id())
                       },
                       #' @description get the value
                       getV = function(){
                         return(private$.decl$e())
                       },
                       #' @description set the value
                       #' @param  val value/expression to be set
                       setV = function(val){
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

#' @title Function Items
#' @description independent functions (that are not part of any other items)
#' in a MiniZinc model
#' @import R6
#' @import checkmate
#' @export
FunctionItem = R6Class("FunctionItem",
                       public = list(
                         #' @description constructor
                         #' @param name name of the function
                         #' @param pars parameter declarations
                         #' @param rt the return type ("bool par", "bool var" or other)
                         #' @param ann annotation
                         #' @param body body of the function
                         initialize = function(name, pars, rt, ann = NULL, body = NULL){
                           stop("under development currently")
                           assertCharacter(name)
                           private$.id = name
                           assertList(pars, "VarDecl")
                           for (i in seq(1, length(pars), 1)) {
                             assertTRUE(pars[[i]]$isPar())
                           }
                           assertTRUE(testNull(ann) || testR6(ann, "Annotation"))
                           private$.ann = ann
                           assertTRUE(testNull(body) || testR6(body, "Expression"))
                           private$.e = body
                         },
                         #' @description get the name of the function
                         name = function(){
                           return(private$.id)
                         },
                         #' @description get the function body
                         b = function(){
                           return(private$.e)
                         },
                         #' @description get the function annotation
                         ann = function(){
                           return(private$.ann)
                         },
                         #' @description get if the function is a test, predicate 
                         #' or a function call itself.
                         rtype = function(){
                           if(private$.rt == "boolPar"){
                             return("test")
                           }else if(private$.rt == "boolVar"){
                             return("predicate")
                           }else{
                             return("function")
                           }
                         },
                         #' @description get the MiniZinc representation
                         c_str = function(){
                           return(sprintf("%s %s;", private$.ann, private$.e))
                         }
                       ),
                       private = list(
                         #' @field .id
                         #' name of the function
                         .id = NULL,
                         #' @field .e
                         #' expression in the function
                         .e = NULL,
                         #' @field .params
                         #' parameter declarations 
                         .params = NULL,
                         #' @field .ann
                         #' annotation 
                         .ann = NULL,
                         #' @field .rt
                         #' return type of the function
                         .rt = NULL
                       ))
