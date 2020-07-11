#' @title Type class
#' 
#' @description the type information of the class
#' 
#' @import R6
#' @import checkmate
#' 
#' @export

Type = R6Class("Type",
               public = list(
                 #' @description constructor
                 #' @param base_type the base type 
                 #' @param kind parameter or decision
                 #' @param type_inst indices or domain 
                 initialize =  function(base_type, kind, type_inst = NULL){
                   assert_choice(base_type, .globals$Type$baseTypes)
                   assert_choice(kind, .globals$Type$kinds)
                   private$.bt = base_type
                   private$.kind = kind
                   if(testR6(type_inst, "TypeInst")){
                     private$.typeInst = type_inst
                   }
                 },
                 #' @description  return the base type
                 bt = function(){
                   return(private$.bt)
                 },
                 #' @description return the kind 
                 kind = function(){
                   return(private$.kind)
                 },
                 ti = function(){
                   return(private$.typeInst)
                 }
                 
               ),
               private = list(
                 #' @field .bt
                 #' the base type
                 .bt = NULL,
                 #' @field .kind
                 #' parameter or decision
                 .kind = NULL,
                 #' @field .dim
                 #' the number of dimensions
                 .dim = 7,
                 #' @field .typeInst
                 #' the type instantiation
                 .typeInst = NULL
               ))