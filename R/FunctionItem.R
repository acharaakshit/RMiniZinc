#' @title Function Items
#' @description independent functions (that are not part of any other items)
#' in a MiniZinc model
#' @import R6
#' @import checkmate
#' @export
FunctionItem = R6Class("FunctionItem",
                       inherit = Item,
                       public = list(
                         #' @description constructor
                         #' @param name name of the function
                         #' @param decls parameter declarations
                         #' @param rt the return type ("bool par", "bool var" or other)
                         #' @param ann annotation
                         #' @param body body of the function
                         initialize = function(name, decls, rt, ann = NULL, body = NULL){
                           assertCharacter(name)
                           private$.id = name
                           assertList(decls, "VarDecl")
                           private$.decls = decls
                           assertTRUE(testNull(ann) || testR6(ann, "Annotation"))
                           private$.ann = ann
                           assertTRUE(testNull(body) || testR6(body, "Expression"))
                           private$.e = body
                           assertR6(rt, "TypeInst")
                           private$.ti = rt
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
                           return(private$.rt)
                         },
                         #' @description get the MiniZinc representation
                         c_str = function(){
                           v_decls = ''
                           for(i in seq(1, length(private$.decls), 1)){
                             v_decls = paste0(v_decls, private$.decls[[i]]$c_str())
                             if(i < length(private$.decls)){
                               v_decls = paste0(v_decls, ", ")
                             }
                           }
                           fnPrefix = ''
                           # set prefix according to type instantiation
                           if(private$.ti$type()$kind() == "par" && private$.ti$type()$bt() == "bool"){
                             fnPrefix = 'test'
                           }else if(private$.ti$type()$kind() == "var" && private$.ti$type()$bt() == "bool"){
                             fnPrefix = 'predicate'
                           }else{
                             # not supported currently
                             fnPrefix = "function "
                             var = ''
                             if(private$.ti$type()$kind() == "var"){
                               var = "var "
                             }
                             
                             if(private$.ti$type()$bt() == "unknown"){
                               # unknown base type -- has a domain
                               if(private$.ti$type()$ndim() == 0 && !private$.ti$type()$isSet()){
                                 fnPrefix = sprintf("%s %s %s: ", fnPrefix, var, private$.ti$getDomain()$c_str()) 
                               }else if(private$.ti$type()$isSet()){
                                 fnPrefix = sprintf("%s %s set of %s: ", fnPrefix, var,
                                                    private$.ti$getDomain()$c_str())
                               }else{
                                 indList = private$.ti$ranges()
                                 indices = ""
                                 for (i in seq(1, length(indList), 1)) {
                                   if(is.character(indList[[i]])){
                                     indices = paste0(indices, is.character(indList[[i]]))  
                                   }else{
                                     indices = paste0(indices, indList[[i]]$c_str()) 
                                   }
                                   
                                   if(i < length(indList)){
                                     indices = paste0(indices, ", ")
                                   }
                                 }
                                 fnPrefix = sprintf("%s array[%s] of %s%s: ", fnPrefix, indices, var, 
                                                    private$.ti$getDomain()$c_str())
                               } 
                             }else{
                               # base type known -- domain doesn't exist
                               if(private$.ti$type()$ndim() == 0 && !private$.ti$type()$isSet()){
                                 fnPrefix = sprintf("%s %s%s:", fnPrefix, var, private$.ti$type()$bt()) 
                               }else if(private$.ti$type()$isSet()){
                                 fnPrefix = sprintf("%s set of %s%s:", fnPrefix, var, private$.ti$type()$bt())
                               }else{
                                 indList = private$.ti$ranges()
                                 indices = ""
                                 for (i in seq(1, length(indList), 1)) {
                                   if(is.character(indList[[i]])){
                                     indices = paste0(indices, "int")  
                                   }else{
                                     indices = paste0(indices, indList[[i]]$c_str()) 
                                   }
                                   if(i < length(indList)){
                                     indices = paste0(indices, ", ")
                                   }
                                 }
                                 bt = ''
                                 if(private$.ti$type()$st()){
                                   bt = sprintf("set of %s", private$.ti$type()$bt())
                                 }else{
                                   bt = private$.ti$type()$bt()
                                 }
                                 fnPrefix = sprintf("%s array[%s] of %s%s:", fnPrefix, indices,
                                                    var, bt)
                               } 
                             }
                           }
                           
                           if(testNull(private$.ann)){
                             return(sprintf("%s %s(%s) = %s;", fnPrefix, private$.id, v_decls,
                                            private$.e$c_str())) 
                           }else{
                             return(sprintf("%s %s %s;", fnPrefix, private$.id, private$.ann$c_str())) 
                           }
                         }
                       ),
                       private = list(
                         #' @field .id
                         #' name of the function
                         .id = NULL,
                         #' @field .e
                         #' expression in the function
                         .e = NULL,
                         #' @field .decls
                         #' parameter declarations 
                         .decls = NULL,
                         #' @field .ann
                         #' annotation 
                         .ann = NULL,
                         #' @field .ti
                         #' return type of the function
                         .ti = NULL
                       ))