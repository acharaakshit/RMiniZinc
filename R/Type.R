#' @title Type class
#' @description 
#' The information of different data types
#' @export

Type = R6Class("Type",
               public = list(
                 #' @description constructor
                 #' @param base_type the base type 
                 #' @param kind parameter or decision
                 #' @param dim the number of dimensions
                 #' @param set_type set or plain
                 initialize =  function(base_type, kind, dim = 0, set_type = FALSE){
                   assert_choice(base_type, .globals$Type$baseTypes)
                   assert_choice(kind, .globals$Type$kinds)
                   private$.bt = base_type
                   private$.kind = kind
                   assert_true(dim >= 0 && dim <=6)
                   private$.dim = dim
                   assert_logical(set_type)
                   private$.st = set_type 
                 },
                 #' @description  return the base type
                 bt = function(){
                   return(private$.bt)
                 },
                 #' @description return if it's set type
                 st = function(){
                   return(private$.st)
                 },
                 #' @description return the kind 
                 kind = function(){
                   return(private$.kind)
                 },
                 #' @description return the number of dimensions
                 ndim = function(){
                   return(private$.dim)
                 },
                 #' @description check if it's an int
                 isInt = function(){
                   return(private$.bt == "int")
                 },
                 #' @description check if it's a float
                 isFloat = function(){
                   return(private$.bt == "float")
                 },
                 #' @description check if it's a bool
                 isBool = function(){
                   return(private$.bt == "bool")
                 },
                 #' @description check if it's a string
                 isString = function(){
                   return(private$.bt == "string")
                 },
                 #' @description return if set in MiniZinc
                 isSet = function(){
                   return(private$.st && private$.dim == 0)
                 },
                 #' @description check if it's a set of int
                 isIntSet = function(){
                   return(private$.st && testTRUE(private$.bt == "int"))
                 },
                 #' @description check if it's a set of float
                 isFloatSet = function(){
                   return(private$.st && testTRUE(private$.bt == "float"))
                 },
                 #' @description check if it's a set of bool
                 isBoolSet = function(){
                   return(private$.st && testTRUE(private$.bt == "bool"))
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
                 #' set or plain
                 .st = NULL
               ))