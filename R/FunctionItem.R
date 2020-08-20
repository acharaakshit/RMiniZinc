#' @title Function Items
#' @description Independent functions (that are not part of any other items)
#' in a MiniZinc model
#' @export
FunctionItem = R6Class("FunctionItem",
                       inherit = Item,
                       public = list(
                         #' @description constructor
                         #' @param name name of the function
                         #' @param decls variable declarations
                         #' @param rt the return type ("bool par", "bool var" or other)
                         #' @param ann annotation
                         #' @param body body of the function
                         #' @param mzn_str string representation of Function Item
                         initialize = function(name = NULL, decls = NULL, rt = NULL,
                                               ann = NULL, body = NULL, mzn_str = NULL){
                           if(testCharacter(mzn_str)){
                             parsedList = suppressWarnings(mzn_parse(modelString = mzn_str))
                             if(!testTRUE(length(parsedList) == 2 &&
                                          names(parsedList$FUNCTION_ITEMS) == "FUNCTION1")){
                               stop("pass only a single function item")
                             }
                             private$.id = parsedList$FUNCTION_ITEMS$FUNCTION1$FUNCTION_NAME
                             v_decls = c()
                             vDecls = parsedList$FUNCTION_ITEMS$FUNCTION1$DETAILS$DECLARATIONS
                             for (j in seq(1, length(vDecls), 1)) {
                               ti = initExpression(vDecls[[j]]["TYPE_INST"])
                               v_decls = c(v_decls, VarDecl$new(name = vDecls[[j]]$NAME, type_inst = ti,
                                                                value = vDecls[[j]]$VALUE))
                             }
                             private$.decls = v_decls
                             private$.ti = initExpression(parsedList$FUNCTION_ITEMS$FUNCTION1$DETAILS["TYPE_INST"])
                             private$.e = initExpression(parsedList$FUNCTION_ITEMS$FUNCTION1$DETAILS$EXPRESSION)
                             private$.ann = initExpression(parsedList$FUNCTION_ITEMS$FUNCTION1$DETAILS["ANNOTATION"])
                           }else{
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
                           }
                         },
                         #' @description get the name of the function
                         name = function(){
                           return(private$.id)
                         },
                         #' @description get the list of declarations
                         decls = function(){
                           return(private$.decls)
                         },
                         #' @description get the function body
                         body = function(){
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