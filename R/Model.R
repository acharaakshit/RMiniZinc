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
                   return(sprintf("include %s;", private$.id))
                 }
               ),
               private = list(
                 #' @field .id
                 #' name of mzn file
                 .id = NULL
               ))

#' @title Assignment Items
#' @description assignments in MiniZinc
#' @import R6
#' @import checkmate
#' @export
AssignItem = R6Class("AssignItem",
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
                         return(sprintf("%s = %s", private$.decl$id(), private$.e$c_str()))
                       }
                     ),
                     private = list(
                       #' @field .decl
                       #' associated declaration
                       .decl = NULL,
                       #' @field .e
                       #' value to be assigned
                       .e = NULL
                     ))
