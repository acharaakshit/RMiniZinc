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
                 #' @param dim the number of dimensions
                 #' @param set_type set or plain
                 initialize =  function(base_type, kind, type_inst = NULL, dim = 0, set_type = FALSE){
                   assert_choice(base_type, .globals$Type$baseTypes)
                   assert_choice(kind, .globals$Type$kinds)
                   private$.bt = base_type
                   private$.kind = kind
                   if(testR6(type_inst, "TypeInst")){
                     private$.typeInst = type_inst
                   }
                   assert_true(dim >= 0 && dim <=6)
                   private$.dim = dim
                   assert_logical(set_type)
                   private$.st = set_type 
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
                 },
                 #' @description return the number of dimensions
                 ndim = function(){
                   return(private$.dim)
                 },
                 #' @description return if set or not
                 isSet = function(){
                   return(private$.st)
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
                 .typeInst = NULL,
                 #' @field .st
                 #' set or plain
                 .st = NULL
               ))