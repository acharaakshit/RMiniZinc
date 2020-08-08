#' @title function to generate an expression 
#' @description This class can be used to represent an expression. 
#' 
#' @import R6
#' @export
Expression = R6Class("Expression",
                         public = list(
                           #' @description constructor
                           initialize = function(){
                             stop(paste(RSmisc::getR6Class(self), "can't be initialized."))  
                           }
                         ))

#' @title create an integer 
#' 
#' @description create an integer in MiniZinc
#' 
#' @import R6 
#' @import checkmate
#' 
#' @export
Int = R6Class("Int", 
               inherit = Expression,
               public = list(
                 #' @description constructor for an int literal
                 #' @param val the value of the integer
                 initialize =  function(val){
                   private$.value = IntVal$new(val = val)
                 },
                 #' @description get the IntVal value
                 getIntVal = function(){
                   return (private$.value$v())
                 },
                 #' @description set the IntVal value
                 setIntVal = function(val){
                   private$.value = IntVal$new(val = val)
                 },
                 #' MiniZinc representation
                 c_str = function(){
                   return(as.character(private$.value$v()))
                 }
               ),
               private = list(
                 #' @field .value
                 #' object of class expression
                 .value = NULL
               ))

#' @title create a float 
#' 
#' @description 
#' create an float in MiniZinc
#' 
#' @import R6 
#' @import checkmate
#' @export
Float = R6Class("Float", 
                inherit = Expression,
                public = list(
                 
                 #' @description constructor for an int literal
                 #' @param val the value of the integer
                 initialize =  function(val){
                   private$.value = FloatVal$new(val = val)
                 },
                 #' @description get the integer value
                 getFloatVal = function(){
                   return (private$.value$v())
                 },
                 #' @description set the integer value
                 setFloatVal = function(val){
                   private$.value = FloatVal$new(val = val)
                 },
                 #' MiniZinc representation
                 c_str = function(){
                   return(as.character(private$.value$v()))
                 }
               ),
               private = list(
                 #' @field .value
                 #' object of class expression
                 .value = NULL
               ))

#' @title create a set
#' 
#' @description 
#' create a set in MiniZinc
#' 
#' @import R6
#' @import checkmate
#' 
#' @export
Set = R6Class("Set",
              inherit = Expression,
              public = list(
                #' @description constuctor
                #' @param val the set value
                #' @param empty_set bool to specify is set is empty(FALSE by default) 
                initialize = function(val = NULL, empty_set = FALSE){
                  if(is.null(val) && empty_set){
                    private$.et = TRUE
                  }else if(testR6(val, "IntSetVal")){
                    private$.isv = val 
                  }else if(testR6(val, "FloatSetVal")){
                    private$.fsv = val
                  }else{
                    assertList(val, "Expression")
                    private$.setVec = val
                  }
                },
                #' @description return the set expression
                getSetVec = function(){
                  return(private$.setVec)
                },
                #' @description set the set expression
                #' @param val list of expressions
                setSetVec = function(val){
                  assertList(val, "Expression")
                  private$.setVec = val
                },
                #' @description return the integer set range
                getIsv = function(){
                  return(private$.isv)
                },
                #' @description set the integer set range
                #' @param val integer set range
                setIsv = function(val){
                  assertR6(val, "IntSetVal")
                  private$.isv = val
                },
                #' @description get the float set range
                getFsv = function(){
                  return(private$.fsv)
                },
                #' @description set the float set range
                #' @param val float set range
                setFsv = function(val){
                  assertR6(val, "FloatSetVal")
                  private$.fsv = val
                },
                #' @description convert into MiniZinc representation
                c_str = function(){
                  if(!is.null(private$.isv)){
                    sprintf("%s..%s", private$.isv$getMin(), private$.isv$getMax())
                  }else if(!is.null(private$.fsv)){
                    sprintf("%s..%s", private$.fsv$getMin(), private$.fsv$getMax())
                  }else{
                    retStr = "";
                    if(private$.et != TRUE){
                      for(i in seq(1, length(private$.setVec), 1)){
                        retStr = paste0(retStr, private$.setVec[[i]]$c_str())
                        if(i < length(private$.setVec)){
                          retStr = paste0(retStr, ", ")
                        }
                      } 
                    }
                    return(sprintf("{%s}", retStr))
                  }
                }
              ),
              private = list(
                #' @field .setVal
                #' the value of the set
                .setVec = NULL,
                #' @field .isv
                #' the integer range set
                .isv = NULL,
                #' @field .fsv
                #' the float range set 
                .fsv = NULL,
                #' @field .et
                #' empty set
                .et = FALSE
              ))

#' @title create a bool
#' 
#' @description 
#' create a bool in MiniZinc
#' 
#' @import R6
#' @import checkmate
#' @export
Bool = R6Class("Bool",
               inherit = Expression,
               public = list(
                 #' @description constructor
                 #' @param val boolean input
                 initialize = function(val){
                   assertLogical(val)
                   private$.value = val 
                 },
                 #' @description get boolean value
                 v = function(){
                    return(private$.value)
                 },
                 #' MiniZinc representation
                 c_str = function(){
                   if(private$.value == TRUE){
                     return("true")  
                   }else{
                     return("false")
                   }
                   
                 }
               ),
               private = list(
                 #' @field .value
                 #' value
                 .value = NULL
               ))

#' @title string lit
#' 
#' @description 
#' create a string in MiniZinc
#' 
#' @import R6
#' @import checkmate
#' @export
String = R6Class("String",
                 inherit = Expression,
                 public = list(
                   #' @description constructor
                   #' @param val string input
                   initialize = function(val){
                     assertCharacter(val)
                     private$.value = val
                   },
                   #' @description get value
                   getV = function(){
                     return(private$.value)
                   },
                   #' @description set value
                   #' @param val string value
                   setV = function(val){
                     assertCharacter(val)
                     private$.value = val
                   },
                   #' MiniZinc representation
                   c_str = function(){
                     return(shQuote(private$.value, "cmd"))
                   }
                 ),
                 private = list(
                   #' @field .value
                   #' string value
                   .value = NULL
                 ))


#' @title Id class (not exposed to the user)
#' 
#' @description create a new Id in MiniZinc
#' @import R6
#' @import checkmate
Id  = R6Class("Id",
              inherit = Expression,
              public = list(
                #' @description constructor
                #' @param id id to be created
                initialize = function(id){
                  assertCharacter(id)
                  private$.id = id
                },
                #' @description get the string identifier
                getId = function(){
                  return(private$.id)
                },
                #' @description set the string identifier
                #' @param val string value to set
                setId = function(val){
                  assertCharacter(val)
                  private$.id = val
                },
                #' @description return the MiniZinc representation
                c_str = function(){
                  return(private$.id)
                }
              ),
              private = list(
                #' @field .id
                #' the string identifier
                .id = NULL
              ))

#' @title create an array 
#' 
#' @description 
#' create an array in MiniZinc
#' 
#' @import R6 
#' @import checkmate
#' @export

Array = R6Class("Array", 
                inherit = Expression,
                public = list(
                   #' @description constructor for an int literal
                   #' @param exprVec the value of the integer
                   #' @param dims dimension expression vector
                   initialize =  function(exprVec, dims = NULL){
                     assert_list(exprVec, "Expression")
                     private$.exprVec = exprVec
                     assertTRUE(testList(dims, "Expression") || testNull(dims))
                     private$.dims = dims 
                   },
                   #' @description get the number of dimensions
                   ndims = function(){
                     return (length(private$.exprVec))
                   },
                   #' @description get the ith dimension vector
                   #' @param i index
                   getDim = function(i){
                     assert_true(i >= 1 && i <= length(dims))
                     return(private$.dims[i])
                   },
                   #' @description return the MiniZinc representation
                   c_str = function(){
                     retStr = ""
                     for (i in seq(1, length(private$.exprVec), 1)) {
                       retStr = paste0(retStr, private$.exprVec[[i]]$c_str())
                       if(i < length(private$.exprVec)){
                         retStr = paste0(retStr, ", ")
                       }
                     }
                     return(sprintf("[%s]", retStr))
                   }
                 ),
                 private = list(
                   #' @field .exprVec
                   #' vector of value expressions
                   .exprVec = NULL,
                   #' @field .dims
                   #' vector of dimension expressions
                   .dims = NULL
                 ))

#' @title Array Access class
#' 
#' @description create ArrayAccess elements
#' 
#' @import R6
#' @import checkmate
#' 
#' @export
ArrayAccess = R6Class("ArrayAccess",
                      inherit = Expression,
                      public = list(
                        #' @description constructor
                        #' @param v the value of element
                        #' @param args the array indices
                        initialize =  function(v, args){
                          assertR6(v, "Expression")
                          private$.v = v
                          assertList(args, "Expression")
                          private$.args = args
                        },
                        #' @description  return the array access value
                        v = function(){
                          return(private$.v)
                        },
                        #' @description return the index id
                        #' @param i index of argument
                        index = function(i){
                          return(private$.index[i])
                        },
                        #' @description return the MiniZinc representation
                        c_str = function(){
                          retStr = ""
                          for (i in seq(1, length(private$.args), 1)) {
                            if(testR6(private$.args[[i]], "VarDecl")){
                              retStr = paste0(retStr, private$.args[[i]]$id()$getId())
                            }else{
                              retStr = paste0(retStr, private$.args[[i]]$c_str()) 
                            }
                            if(i < length(private$.args)){
                              retStr = paste0(retStr, ", ")
                            }
                          }
                          return(sprintf("%s[%s]", private$.v$c_str(), retStr))
                        }
                      ),
                      private = list(
                        #' @field .v
                        #' the id/value of array
                        .v = NULL,
                        #' @field .index
                        #' indices of the array
                        .args = NULL
                      )
                      )

#' @title Generator class
#' 
#' @description create a generator
#' @import R6
#' @import checkmate
#' @export 
Generator = R6Class("Generator",
                    inherit = Expression,
                     public = list(
                       #' @description constructor
                       #' @param IN the in expression of generator
                       #' @param where the where expression of generator
                       #' @param decl list of variable declarations
                       initialize = function(IN = NULL, where = NULL, decls){
                         assert_true(testR6(IN, "Expression") || testNull(IN))
                         assert_true(testR6(where, "Expression") || testNull(where))
                         private$.in = IN
                         private$.where = where
                         assert_list(decls, "VarDecl")
                         private$.decls = decls
                       },
                       #' @description get the in expression
                       In = function(){
                         return(private$.in)
                       },
                       #' @description get the where expression
                       where = function(){
                         return(private$.where)
                       },
                       #' @description get the ith declaration
                       #' @param i index
                       decl = function(i){
                         return(private$.decls[[i]])
                       },
                       #' @description get the MiniZinc representation
                       c_str = function(){
                         dStr = ""
                         for(i in seq(1, length(private$.decls), 1)){
                           dStr = paste0(dStr, private$.decls[[i]]$id()$getId())
                           if(i < length(private$.decls)){
                             dStr = paste0(dStr, ",")
                           }
                         }
                         inStr = ''
                         if(!is.null(private$.in)){
                           inStr = sprintf("in %s", private$.in$c_str())
                         }
                         whereStr = ''
                         if(!is.null(private$.where)){
                           whereStr = sprintf("where %s", private$.where$c_str())
                         }
                         return(sprintf("%s %s %s", dStr, inStr, whereStr)) 
                       }
                     ),
                     private = list(
                       #' @field .decls
                       #' variable declarations
                       .decls = NULL,
                       #' @field .in
                       #' in expression
                       .in = NULL,
                       #' @field where
                       #' where expression
                       .where = NULL
                     ))

#' @title Comprehension class
#' 
#' @description create a Comprehension
#' @import R6
#' @import checkmate
#' @export 
Comprehension = R6Class("Comprehension",
                        inherit = Expression,
                         public = list(
                           #' @description constructor
                           #' @param generators generators of the expression
                           #' @param e inside the comprehension
                           #' @param set bool to specify if comprehension is a set.
                           initialize = function(generators, e, set){
                             assert_list(generators, "Generator")
                             private$.generators = generators
                             assertR6(e, "Expression")
                             private$.e = e
                             assertLogical(set)
                             private$.set = set
                           },
                           #' @description get the number of generators
                           ngens = function(){
                             return(length(private$.generators))
                           },
                           #' @description get the ith generator expression
                           #' @param i index
                           gen_i = function(i){
                             return(private$.generators[[i]])
                           },
                           #' @description get the in expression of ith generator
                           #' @param i index
                           In = function(i){
                             return(private$.generators[[i]]$In())
                           },
                           #' @description get the where expression of ith generator
                           #' @param i index
                           where = function(i){
                             return(private$.generators[[i]]$where())
                           },
                           #' @description get the idecl declaration of igen generator
                           #' @param igen generator index
                           #' @param idecl declaration index
                           decl = function(igen, idecl){
                             return(private$.generators[[igen]]$decl(idecl))
                           },
                           #' @description get the expression
                           e = function(){
                             return(private$.e)
                           },
                           #' @description check if comprehension is a set
                           isSet = function(){
                             return(private$.set)
                           },
                           #' @description get the MiniZinc representation
                           c_str = function(){
                             gStr = ""
                             for (i in seq(1, length(private$.generators), 1)) {
                               gStr = paste0(gStr, private$.generators[[i]]$c_str())
                               if(i < length(private$.generators)){
                                 gStr = paste0(gStr, ", ")
                               }
                             }
                             if(private$.set){
                               return(sprintf("{%s | %s}", private$.e$c_str(), gStr)) 
                             }else{
                               return(sprintf("[%s | %s]", private$.e$c_str(), gStr))  
                             }
                           }
                         ),
                         private = list(
                           #' @field .generators
                           #' a vector of generators
                           .generators = NULL,
                           #' @field .expression
                           #' the comprehension expression
                           .e = NULL,
                           #' @field .set
                           #' TRUE if comprehension is a set
                           .set = NULL
                         ))

#' @title Binop class
#' 
#' @description create a binary operator expression
#' @import R6
#' @import checkmate
#' @export 
BinOp = R6Class("BinOp",
                inherit = Expression,
                 public = list(
                   #' @description constructor
                   #' @param lhs_expression the left hand side expression
                   #' @param binop the binary operator to be used
                   #' @param rhs_expression the right hand side expression
                   initialize  = function(lhs_expression, binop, rhs_expression){
                        assertR6(lhs_expression, "Expression")
                        assertR6(rhs_expression, "Expression")
                        assert_choice(binop, .globals$binopTypes)
                        private$.lhs_exp = lhs_expression
                        private$.rhs_exp = rhs_expression
                        private$.op = binop
                    },
                   #' @description return the lhs expression
                   lhs =  function(){
                     return(private$.lhs_exp)
                   },
                   #' @description return the rhs expression
                   rhs = function(){
                     return(private$.rhs_exp)
                   },
                   #' @description return the operator
                   op =  function(){
                     return(private$.op)
                   },
                   #' @description return the MiniZinc representation
                   c_str = function(){
                     return(sprintf("%s %s %s", private$.lhs_exp$c_str(),
                                    private$.op, private$.rhs_exp$c_str()))
                   }
                 ),
                private = list(
                  #' @field .lhs_exp
                  #' the left hand side expression
                  .lhs_exp = NULL,
                  #' @field .rhs_exp
                  #' the right hand side expression
                  .rhs_exp = NULL,
                  #' @field .op
                  #' the operator
                  .op = NULL
                ))


#' @title unOp class
#' @description Unary operator in MiniZinc
#' @import R6
#' @import checkmate
#' @export
UnOp = R6Class("UnOp",
               public = list(
                 #' @description constructor
                 #' @param args list of expressions
                 #' @param op unary operator
                 initialize = function(args, op){
                   assertR6(args, "Expression")
                   private$.args = args
                   assert_choice(op, .globals$unopTypes)
                   private$.op = op
                 },
                 #' @description get the number of arguments
                 nargs = function(){
                   return(length(private$.args))
                 },
                 #' @description get the ith expression argument
                 #' @param i index
                 arg = function(i){
                   return(private$.args[[i]])
                 },
                 op = function(){
                   return(private$.op)
                 },
                 #' @description return the MiniZinc representation
                 c_str =  function(){
                   uoStr = ''
                   for(i in seq(1, length(private$.args), 1)){
                     uoStr = paste0(uoStr, args[[i]]$c_str())
                     if(i < length(private$.args)){
                       uoStr = paste0(uoStr, ", ")
                     }
                   }
                   return(sprintf("%s(%s)", private$.op, uoStr))
                 }
               ),
               private = list(
                 #' @field .argExps
                 #' list of expression arguments
                 .argExps = NULL,
                 #' @field .op
                 #' operator to be used
                 .op = NULL
               ))

#' @title Call class
#' 
#' @description function calls in MiniZinc
#' 
#' @import R6
#' @import checkmate
#' 
#' @export
Call = R6Class("Call",
               inherit = Expression,
               public = list(
                 #' @description constructor
                 #' @param fn_id the function id
                 #' @param argExps the list of expressions
                 initialize =  function(fnName , args){
                   assert_string(fnName)
                   private$.id = fnName
                   assert_list(args, "Expression")
                   private$.args = args
                   private$.nargs = length(args)
                 },
                 #' @description the function id/string
                 id =  function(){
                   return(private$.id)
                 },
                 #' @description get the number of arguments
                 nargs = function(){
                   return(private$.nargs)
                 },
                 #' @description get the expression based on index
                 #' @param i index 
                 arg = function(i){
                   return(private$.args[[i]])
                 },
                 #' @description set argument i
                 #' @param i index
                 #' @param e  expression
                 setArg = function(e, i){
                   assertR6(e, "Expression")
                   private$.args[[i]] = e
                 },
                 #' @description set all the expression arguments
                 #' @param expList
                 setArgs = function(expList){
                   assertList(expList, "Expression")
                   private$.args = expList
                 },
                 #' @description return the MiniZinc representation
                 c_str = function(){
                   clStr = ""
                   for(i in seq(1, self$nargs(), 1)){
                     clStr = paste0(clStr, private$.args[[i]]$c_str())
                     if(i < self$nargs()){
                       clStr = paste0(clStr, ", ")
                     }
                   }
                   return(sprintf("%s(%s)", private$.id, clStr))
                 }
               ),
               private = list(
                 #' @field .id
                 #' the function id
                 .id = NULL,
                 #' @field .lExp
                 #' list of expressions
                 .args = NULL,
                 #' @field .nargs
                 #' number of arguments to the call
                 .nargs = NULL
               ))

#' @title let expression
#' @description let expression in MiniZinc
#' @import R6
#' @import checkmate
#' @export
Let = R6Class("Let",
              inherit = Expression,
              public = list(
                #' @description constructor
                #' @param let list of local declarations
                #' @param body body of the let  
                initialize = function(let, body){
                  assertList(let, "Expression")
                  assertR6(body, "Expression")
                  private$.decl = let
                  private$.in = body
                },
                #' @description  access list of local declarations
                let = function(){
                  return(private$.let)
                },
                #' @description return the body
                body = function(){
                  return(private$.in)
                },
                #' @description get the MiniZinc representation
                c_str = function(){
                  declStr = ""
                  for(i in seq(1, length(private$.decl), 1)){
                    declStr = paste0(declStr, private$.decl[[i]]$c_str())
                    if(i < length(private$.decl)){
                      declStr = paste0(declStr, ", ")
                    }
                  }
                  return(sprintf("let {%s} in %s", declStr, private$.in$c_str()))
                }
              ),
              private = list(
                #' @field .decl
                #' list of local declarations
                .decl = NULL,
                #' @field .in
                #' body of the let
                .in = NULL
              ))

#' @title ITE expression
#' @description if-then-else expressions in MiniZinc
#' @import R6
#' @import checkmate
#' @export
Ite = R6Class("Ite",
              inherit = Expression,
              public = list(
                #' @description constructor
                #' @param ifs list of if expressions
                #' @param thens list of corresponding then expressions
                #' @param Else else expression
                initialize = function(ifs ,thens, Else){
                  assertList(ifs, "Expression")
                  assertList(thens, "Expression")
                  assert_true(length(ifs) == length(thens))
                  assertR6(Else, "Expression")
                  private$.ifs = ifs
                  private$.thens = thens
                  private$.else = Else
                },
                #' @description get the ith if expression
                #' @param i index
                If = function(i){
                  return(private$.ifs[[i]])
                },
                #' @description get the ith then expression
                #' @param i index
                Then = function(i){
                  return(private$.thens[[i]])
                },
                #' @description get the else expression
                Else = function(){
                  return(private$.else)
                },
                #' @description get the MiniZinc representation
                c_str = function(){
                  ifthenStr = ''
                  for(i in seq(1, length(private$.ifs), 1)){
                    ifthenStr = paste0(ifthenStr, sprintf("if (%s) then (%s) else", private$.ifs[[i]]$c_str(),
                                        private$.thens[[i]]$c_str()))
                  }
                  return(sprintf("%s (%s) endif", ifthenStr, private$.else$c_str()))
                }
              ),
              private = list(
                #' @field .ifs
                #' list of if expressions
                .ifs = NULL,
                #' @field .thens
                #' list of corresponding then expressions
                .thens = NULL,
                #' @field .else
                #' else expression
                .else = NULL
              ))

#' @title class for variable declaration
#' 
#' @description 
#' Contains different fields to create a variable declaration
#' 
#' @import R6 
#' @import checkmate
#' 
#' @export
VarDecl = R6Class("VarDecl",
                  inherit = Expression,
                  public = list(
                    #' @description initialize the VarDecl constructor
                    #' @param type Type object
                    #' @param e value, NULL by default
                    #' @param id the id/name.
                    initialize = function(e = NULL, id, type_inst){
                      assertR6(type_inst, "TypeInst")
                      private$.ti = type_inst
                      if(testR6(e, "Expression")){
                        private$.e  =  e
                      }
                      assert_string(id)
                      private$.id = Id$new(id)
                    },
                    #' @description 
                    #' the identifier
                    id = function(){
                      return(private$.id)
                    },
                    #' @description check if it's a parameter
                    isPar = function(){
                      if(private$.ti$type()$kind() == "par"){
                        return (TRUE)
                      }
                      return(FALSE)
                    },
                    #' @description check if it's a decision variable
                    isVar = function(){
                      if(private$.ti$type()$kind() == "var"){
                        return (TRUE)
                      }
                      return(FALSE)
                    },
                    #' @description return string representation of MiniZinc
                    c_str = function(){
                      retStr = ""
                      var = ""
                      if (self$isVar()){
                        var = "var "  
                      }
                      if(private$.ti$type()$bt() == "ann"){
                        retStr = sprintf("ann: %s",  private$.id$getId())
                      }else if(private$.ti$type()$bt() == "unknown"){
                        if(private$.ti$type()$ndim() == 0 && !private$.ti$type()$isSet()){
                          retStr = sprintf("%s%s: %s", var, private$.ti$domain()$c_str(), private$.id$getId()) 
                        }else if(private$.ti$type()$isSet()){
                          retStr = sprintf("%s set of %s: %s",var, private$.ti$domain()$c_str(),
                                           private$.id$getId())
                        }else{
                          indList = private$.ti$ranges()
                          indices = ""
                          for (i in seq(1, length(indList), 1)) {
                            indices = paste0(indices, indList[[i]]$c_str())
                            if(i < length(indList)){
                              indices = paste0(indices, ", ")
                            }
                          }
                          retStr = sprintf("array[%s] of %s%s: %s", indices, var, 
                                           private$.ti$domain()$c_str(), private$.id$getId())
                        } 
                      }else{
                        if(private$.ti$type()$ndim() == 0 && !private$.ti$type()$isSet()){
                          retStr = sprintf("%s%s: %s", var, private$.ti$type()$bt(), private$.id$getId()) 
                        }else if(private$.ti$type()$isSet()){
                          retStr = sprintf("set of %s%s: %s",private$.ti$type()$bt(),
                                           var, private$.id$getId())
                        }else{
                          indList = private$.ti$ranges()
                          indices = ""
                          for (i in seq(1, length(indList), 1)) {
                            indices = paste0(indices, indList[[i]]$c_str())
                            if(i < length(indList)){
                              indices = paste0(indices, ", ")
                            }
                          }
                          retStr = sprintf("array[%s] of %s%s: %s", indices,
                                           var, private$.ti$type()$bt(), private$.id$getId())
                        } 
                      }
                      if(!is.null(private$.e)){
                        return(sprintf("%s = %s", retStr, private$.e$c_str()))
                      }
                      return(sprintf("%s", retStr))
                    },
                    #' @description return the initialization expression
                    e = function(){
                      return(private$.e)
                    },
                    #' @description type of the variable declaration
                    ti = function(){
                      return(private$.ti)
                    }
                  ),
                  private = list(
                    #' @field .ti
                    #' type instantiation information
                    .ti = NULL,
                    #' @field id
                    #' name of the variable
                    .id = NULL,
                    #' @field .expression
                    #' the initialization expression
                    .e = NULL
                  ))


#' @title TypeInst class
#' 
#' @description type instantiation with indices, etc.
#' 
#' @import R6
#' @import checkmate
#' 
#' @export
TypeInst = R6Class("TypeInst",
                   inherit = Expression,
                   public = list(
                     #' @description constuctor
                     #' @param type type of declaration
                     #' @param indexExprVec the expression vector of indices
                     #' @param domain the domain of decision variables
                     initialize = function(type, indexExprVec = NULL, domain = NULL){
                       assertR6(type, "Type")
                       private$.type = type 
                       assertTRUE(testList(indexExprVec, "Expression") || testNull(indexExprVec))
                       private$.indExpr = indexExprVec
                       assertTRUE(testR6(domain, "Expression") || testNull(domain))
                       private$.domain = domain
                     },
                     #' @description return the domain
                     domain = function(){
                       return(private$.domain)
                     }, 
                     #' @description return the index expression vector
                     ranges = function(){
                       return(private$.indExpr)
                     },
                     #' @description check if it's an array
                     isArray = function(){
                       if(dim>=1 && private$.type$isSet() == FALSE){
                          return(TRUE)
                       }
                       return(FALSE)
                     },
                     #' @description return the type information
                     type = function(){
                       return(private$.type)
                     }
                       
                   ),
                   private = list(
                     #' @field .indExpr
                     #' the index expression
                     .indExpr = NULL,
                     #' @field .domain
                     #' the domain of possible values to be taken
                     .domain = NULL,
                     #' @field .type
                     #' the type information
                     .type = NULL
                   ))

#' @title Annotation
#' @description create Annotations in MiniZinc
#' @import R6
#' @import checkmate
#' @export
Annotation = R6Class("Annotation",
                     public = list(
                       #' @description constructor
                       #' @param expVec expression vector
                       initialize = function(expVec){
                        assertList(expVec, "Expression")
                         private$.expVec = expVec
                       },
                       #' @description get the list of expressions
                       getExp = function(){
                         return(private$.expVec)
                       },
                       #' @description get the MiniZinc expression
                       c_str = function(){
                         retStr = ""
                         for (i in seq(1, length(private$.expVec), 1)) {
                           retStr = paste0(retStr, private$.expVec[[i]]$c_str())
                         }
                         return(sprintf(" :: %s", retStr))
                       }
                     ),
                     private = list(
                       #' @field .expVec
                       #' list of expressions
                       .expVec = NULL
                     ))
