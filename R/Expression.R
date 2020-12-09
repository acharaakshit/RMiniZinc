#' @title Expression (Abstract class -- should not be initialized) 
#' @description 
#' This class represents an expression in MiniZinc. 
#' @export
Expression = R6Class("Expression",
                         public = list(
                           #' @description constructor
                           initialize = function(){
                             stop(paste(RSmisc::getR6Class(self), "can't be initialized."))  
                           }
                         ))

#' @title Int 
#' @description 
#' Create an integer in MiniZinc
#' @export
#' @examples 
#' newInt = Int$new(10)
#' newInt$c_str()
#' newInt$setIntVal(newInt$getIntVal() + 20)
#' newInt$c_str()
Int = R6Class("Int", 
               inherit = Expression,
               public = list(
                 #' @description constructor
                 #' @param val the value of the integer
                 initialize =  function(val){
                   private$.value = IntVal$new(val = val)
                 },
                 #' @description get the IntVal value
                 getIntVal = function(){
                   return (private$.value$v())
                 },
                 #' @description set the IntVal value
                 #' @param val value to be set
                 setIntVal = function(val){
                   private$.value = IntVal$new(val = val)
                 },
                 #' @description get the MiniZinc representation
                 c_str = function(){
                   return(format(private$.value$v(), scientific = FALSE))
                 }
               ),
               private = list(
                 #' @field .value
                 #' object of class expression
                 .value = NULL
               ))

#' @title Float 
#' @description 
#' Create a float in MiniZinc
#' @export
#' @examples 
#' newFloat = Float$new(1.5)
#' newFloat$c_str()
#' newFloat$setFloatVal(newFloat$getFloatVal() + 2.5)
#' newFloat$c_str()
Float = R6Class("Float", 
                inherit = Expression,
                public = list(
                 #' @description constructor
                 #' @param val the float value
                 initialize =  function(val){
                   private$.value = FloatVal$new(val = val)
                 },
                 #' @description get the float value
                 getFloatVal = function(){
                   return (private$.value$v())
                 },
                 #' @description set the float value
                 #' @param val value to be set
                 setFloatVal = function(val){
                   private$.value = FloatVal$new(val = val)
                 },
                 #' @description get the MiniZinc representation
                 c_str = function(){
                   return(as.character(private$.value$v()))
                 }
               ),
               private = list(
                 #' @field .value
                 #' object of class expression
                 .value = NULL
               ))

#' @title Bool
#' @description 
#' Create a bool in MiniZinc
#' @export
#' @examples 
#' newBool = Bool$new(TRUE)
#' newBool$c_str()
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
                 #' @description get the MiniZinc representation
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

#' @title String
#' @description 
#' Create a string in MiniZinc
#' @export
#' @examples 
#' newString = String$new("example")
#' newString$c_str()
#' newString$setV("new example")
#' newString$c_str()
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
                   #' @description get the MiniZinc representation
                   c_str = function(){
                     return(shQuote(private$.value, "cmd"))
                   }
                 ),
                 private = list(
                   #' @field .value
                   #' string value
                   .value = NULL
                 ))

#' @title Set
#' @description 
#' Create a set in MiniZinc
#' @import R6
#' @import checkmate
#' @export
#' @examples 
#' newIntSet = Set$new(val = IntSetVal$new(1,5))
#' newIntSet$c_str()
#' newIntSet$setIsv(IntSetVal$new(2,6))
#' newIntSet$c_str()
#' newFloatSet = Set$new(val = FloatSetVal$new(1.1,5.1))
#' newFloatSet$c_str()
#' newFloatSet$setFsv(FloatSetVal$new(1.2,4.1))
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
                #' @description get the set expression
                getSetVec = function(){
                  return(private$.setVec)
                },
                #' @description set the set expression
                #' @param val list of expressions
                setSetVec = function(val){
                  assertList(val, "Expression")
                  private$.setVec = val
                },
                #' @description is the set empty
                isEmpty = function(){
                  return(private$.et)
                },
                #' @description make the set empty
                makeEmpty = function(){
                  private$.setVec = NULL
                  private$.et = TRUE
                },
                #' @description return the integer set range
                getIsv = function(){
                  return(private$.isv)
                },
                #' @description set the integer set range
                #' @param val integer set range
                setIsv = function(val){
                  assertR6(val, "IntSetVal")
                  assertTRUE(!testNull(private$.isv))
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
                  assertTRUE(!testNull(private$.fsv))
                  private$.fsv = val
                },
                #' @description get the MiniZinc representation
                c_str = function(){
                  if(!is.null(private$.isv)){
                    return(sprintf("%s..%s",private$.isv$getMin(), private$.isv$getMax()))
                  }else if(!is.null(private$.fsv)){
                    return(sprintf("%s..%s",private$.fsv$getMin(), private$.fsv$getMax()))
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
                },
                #' @description delete flag for internal use
                getDeleteFlag = function(){
                  return(private$.delete_flag)
                },
                #' @description delete the assignment item
                delete = function(){
                  private$.delete_flag = TRUE
                  helperDeleteExpression("Set")
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
                .et = FALSE,
                #' @field .delete_flag
                #' used to delete items
                .delete_flag = FALSE
              ))

#' @title create an array 
#' @description 
#' Create an array in MiniZinc
#' @export
#' @examples 
#' newArray = Array$new(exprVec = c(Int$new(1), Int$new(2)))
#' newArray$c_str()
Array = R6Class("Array", 
                inherit = Expression,
                public = list(
                   #' @description constructor for an int literal
                   #' @param exprVec list of expressions in the array
                   #' @param dimranges list of min and max index of each dimension
                   initialize =  function(exprVec, dimranges = NULL){
                     assert_list(exprVec, "Expression")
                     private$.exprVec = exprVec
                     if(testNull(dimranges)){
                       message("dimensions not provided: initializing as 1d Array with
                               min index 1 and max index <number_of_elements>")
                       private$.dims = list(IntSetVal$new(imin = 1, imax = length(exprVec)))
                     }else{
                       assertList(dimranges, "IntSetVal") 
                       assertTRUE(length(dimranges) > 0)
                       if(length(dimranges) > 2){
                         stop("3 or higher dimensional sliced array literals not supported")
                       }
                       dim_sum = 1
                       for (i in seq(1, length(dimranges), 1)) {
                           x = dimranges[[i]]
                           dim_sum = dim_sum * (x$getMax() - x$getMin() + 1)
                        }
                       assertTRUE(dim_sum == length(exprVec))
                       private$.dims = dimranges 
                     }
                   },
                   #' @description get the number of dimensions
                   ndims = function(){
                     return (length(private$.dims))
                   },
                   #' @description get the minimum index of dimension i
                   #' @param i ith dimension 
                   getMinIndex = function(i){
                     return (private$.dims[[i]]$getMin())
                   },
                   #' @description get the maximum index of dimension i
                   #' @param i ith dimension
                   getMaxIndex = function(i){
                     return (private$.dims[[i]]$getMax())
                   },
                   #' @description set the minimum index of dimension i
                   #' @param i dimension number
                   #' @param minIndex integer for min index
                   setMinIndex = function(i, minIndex){
                     private$.dims[[i]]$setMin(i)
                   },
                   #' @description set the maximum index of dimension i
                   #' @param i dimension number
                   #' @param maxIndex integer for max index
                   setMaxIndex = function(i, maxIndex){
                     private$.dims[[i]]$setMax(i)
                   },
                   #' @description get the ith element from vector
                   #' @param i index
                   getVal = function(i){
                     return(private$.exprVec[[i]])
                   },
                   #' @description set the ith element from vector
                   #' @param i index
                   #' @param val value of expression to be set
                   setVal = function(i, val){
                     assertR6(val, "Expression")
                     private$.exprVec[[i]] = val
                   },
                   #' @description return the MiniZinc representation
                   c_str = function(){
                     retStr = ""
                     if(length(private$.dims) == 2){
                       retStr = "|"
                       dim1 = private$.dims[[1]]$getMax() - private$.dims[[1]]$getMin() + 1
                       dim2 = private$.dims[[2]]$getMax() - private$.dims[[2]]$getMin() + 1
                       slice_flag = 1
                       for (i in seq(1, length(private$.exprVec), 1)) {
                           if(slice_flag == 2){
                              slice_flag = 1 
                           }
                           retStr = paste0(retStr, private$.exprVec[[i]]$c_str())
                           if((i %% dim2 == 0 && slice_flag %% 2 != 0) || 
                              (i %% dim1 == 0 && slice_flag %% 2 == 0)){
                             slice_flag = 2
                             retStr = paste0(retStr, "\n|")
                           }else{
                             retStr = paste0(retStr, ", ")
                           }
                         }
                     }else{
                       for (i in seq(1, length(private$.exprVec), 1)) {
                         retStr = paste0(retStr, private$.exprVec[[i]]$c_str())
                         if(i < length(private$.exprVec)){
                           retStr = paste0(retStr, ", ")
                         }
                       }
                      }
                       
                    return(sprintf("[%s]", retStr))
                   },
                   #' @description delete flag for internal use
                   getDeleteFlag = function(){
                     return(private$.delete_flag)
                   },
                   #' @description delete the assignment item
                   delete = function(){
                     private$.delete_flag = TRUE
                     helperDeleteExpression("Array")
                   }
                 ),
                 private = list(
                   #' @field .exprVec
                   #' vector of value expressions
                   .exprVec = NULL,
                   #' @field .dims
                   #' vector of dimension expressions
                   .dims = NULL,
                   #' @field .delete_flag
                   #' used to delete items
                   .delete_flag = FALSE
                 ))

#' @title Id class (not exposed to the user)
#' @description 
#' Create a new Id in MiniZinc
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
                getName = function(){
                  return(private$.id)
                },
                #' @description set the string identifier
                #' @param name string name to set
                setName = function(name){
                  assertCharacter(name)
                  private$.id = name
                },
                #' @description return the MiniZinc representation
                c_str = function(){
                  return(private$.id)
                },
                #' @description delete flag for internal use
                getDeleteFlag = function(){
                  return(private$.delete_flag)
                },
                #' @description delete the assignment item
                delete = function(){
                  private$.delete_flag = TRUE
                  helperDeleteExpression("Id")
                }
              ),
              private = list(
                #' @field .id
                #' the string identifier
                .id = NULL,
                #' @field .delete_flag
                #' used to delete items
                .delete_flag = FALSE
              ))


#' @title Array Access
#' @description 
#' Create ArrayAccess elements in MiniZinc
#' @export
#' @examples 
#' vDecl1 = IntSetDecl(name = "SET", kind = "par")
#' vDecl2 = IntArrDecl(name = "profit", kind = "par", ndim = 1, 
#' ind = list(vDecl1$getId()))
#' newArrayAccess = ArrayAccess$new(v = vDecl2$getId(),
#'  args = list(IntDecl(name = "i", kind = "par")))
ArrayAccess = R6Class("ArrayAccess",
                      inherit = Expression,
                      public = list(
                        #' @description constructor
                        #' @param v the value/identifier of variable decl
                        #' @param args the array indices
                        initialize =  function(v, args){
                          assertR6(v, "Id")
                          private$.v = v
                          assertList(args, "Expression")
                          private$.args = args
                        },
                        #' @description  get the array access value
                        getV = function(){
                          return(private$.v)
                        },
                        #' @description  set the array access value
                        #' @param val new array access value
                        setV = function(val){
                          assertR6(val, "Id")
                          private$.v = val
                        },
                        #' @description get the number of arguments
                        nargs = function(){
                          return(length(private$.args))
                        },
                        #' @description get the arguments
                        getArgs = function(){
                          return(private$.args)
                        },
                        #' @description set the arguments
                        #' @param val new arguments
                        setArgs = function(val){
                          assertList(val, "Expression")
                          private$.args = val
                        },
                        #' @description return the MiniZinc representation
                        c_str = function(){
                          retStr = ""
                          for (i in seq(1, length(private$.args), 1)) {
                            retStr = paste0(retStr, private$.args[[i]]$c_str()) 
                            if(i < length(private$.args)){
                              retStr = paste0(retStr, ", ")
                            }
                          }
                          return(sprintf("%s[%s]", private$.v$c_str(), retStr))
                        },
                        #' @description delete flag for internal use
                        getDeleteFlag = function(){
                          return(private$.delete_flag)
                        },
                        #' @description delete the assignment item
                        delete = function(){
                          private$.delete_flag = TRUE
                          helperDeleteExpression("ArrayAccess")
                        }
                      ),
                      private = list(
                        #' @field .v
                        #' the id/value of array
                        .v = NULL,
                        #' @field .args
                        #' arguments of the array
                        .args = NULL,
                        #' @field .delete_flag
                        #' used to delete items
                        .delete_flag = FALSE
                      )
                      )

#' @title Generator 
#' @description 
#' Create a generator in MiniZinc
#' @export 
#' @examples 
#' newGen = Generator$new(IN = IntSetDecl(name = "SET", kind = "par"), 
#' decls = list(IntDecl(name = "i", kind = "par")))
Generator = R6Class("Generator",
                    inherit = Expression,
                     public = list(
                       #' @description constructor
                       #' @param decls list of variable declarations
                       #' @param IN the in expression of generator
                       #' @param where the where expression of generator
                       initialize = function(decls, IN = NULL, where = NULL){
                         assert_true(testR6(IN, "Expression") || testNull(IN))
                         assert_true(testR6(where, "Expression") || testNull(where))
                         private$.in = IN
                         private$.where = where
                         assert_list(decls, "VarDecl")
                         private$.decls = decls
                       },
                       #' @description get the in expression
                       getIn = function(){
                         return(private$.in)
                       },
                       #' @description set the in expression
                       #' @param expIn expression to be set 
                       setIn = function(expIn){
                         assertR6(expIn, "Expression")
                         private$.in = expIn
                       },
                       #' @description get the where expression
                       getWhere = function(){
                         return(private$.where)
                       },
                       #' @description get the where expression
                       #' @param expWhere where expression (or NULL)
                       setWhere = function(expWhere){
                         assertTRUE(testR6(expWhere, "Expression") ||
                                      testNull(expWhere))
                         private$.where = expWhere
                       },
                       #' @description get the ith declaration
                       #' @param i index
                       getDecl = function(i){
                         return(private$.decls[[i]])
                       },
                       #' @description get the ith declaration
                       #' @param i index
                       #' @param decl declaration to be set
                       setDecl = function(i, decl){
                         assertR6(decl, "VarDecl")
                         private$.decls[[i]] = decl
                       },
                       #' @description get the MiniZinc representation
                       c_str = function(){
                         dStr = ""
                         for(i in seq(1, length(private$.decls), 1)){
                           dStr = paste0(dStr, private$.decls[[i]]$getId()$getName())
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
                       },
                       #' @description delete flag for internal use
                       getDeleteFlag = function(){
                         return(private$.delete_flag)
                       },
                       #' @description delete the assignment item
                       delete = function(){
                         private$.delete_flag = TRUE
                         helperDeleteExpression("Generator")
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
                       .where = NULL,
                       #' @field .delete_flag
                       #' used to delete items
                       .delete_flag = FALSE
                     ))

#' @title Comprehension
#' @description 
#' Create a Comprehension in MiniZinc
#' @export 
Comprehension = R6Class("Comprehension",
                        inherit = Expression,
                         public = list(
                           #' @description constructor
                           #' @param generators generators of the expression
                           #' @param body body/expression of the comprehension
                           #' @param set bool to specify if comprehension is a set.
                           initialize = function(generators, body, set){
                             assert_list(generators, "Generator")
                             private$.generators = generators
                             assertR6(body, "Expression")
                             private$.e = body
                             assertLogical(set)
                             private$.set = set
                           },
                           #' @description get the number of generators
                           ngens = function(){
                             return(length(private$.generators))
                           },
                           #' @description get all the generator expressions
                           getGens = function(){
                             return(private$.generators)
                           },
                           #' @description set all the generator expressions
                           #' @param generators list of generator expressions to be set
                           setGens = function(generators){
                             assert_list(generators, "Generator")
                             private$.generators = generators
                           },
                           #' @description get the ith generator expression
                           #' @param i index
                           getGen = function(i){
                             return(private$.generators[[i]])
                           },
                           #' @description set the ith generator expression
                           #' @param i index
                           #' @param expGen generator expression to be set
                           setGen = function(i, expGen){
                             assertR6(expGen, "Generator")
                             private$.generators[[i]] = expGen
                           },
                           #' @description get the expression/body
                           getBody = function(){
                             return(private$.e)
                           },
                           #' @description set the expression/body
                           #' @param e new expression value
                           setBody = function(e){
                             assertR6(e, "Expression")
                             private$.e = e
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
                           },
                           #' @description delete flag for internal use
                           getDeleteFlag = function(){
                             return(private$.delete_flag)
                           },
                           #' @description delete the assignment item
                           delete = function(){
                             private$.delete_flag = TRUE
                             helperDeleteExpression("Comprehension")
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
                           .set = NULL,
                           #' @field .delete_flag
                           #' used to delete items
                           .delete_flag = FALSE
                         ))

#' @title BinOp 
#' @description 
#' Create a binary operation expression
#' @export 
#' @examples
#' newBinOp = BinOp$new(lhs = Int$new(2), binop = "+", rhs = Int$new(5))
#' newBinOp$c_str()
#' newBinOp$setLhs(Int$new(5))
#' newBinOp$setOp("-")
#' newBinOp$setRhs(Int$new(2))
#' newBinOp$c_str()
BinOp = R6Class("BinOp",
                inherit = Expression,
                 public = list(
                   #' @description constructor
                   #' @param lhs the left hand side expression
                   #' @param binop the binary operator to be used
                   #' @param rhs the right hand side expression
                   initialize  = function(lhs, binop, rhs){
                        assertR6(lhs, "Expression")
                        assertR6(rhs, "Expression")
                        assert_choice(binop, .globals$binopTypes)
                        private$.lhs_exp = lhs
                        private$.rhs_exp = rhs
                        private$.op = binop
                    },
                   #' @description get the lhs expression
                   getLhs =  function(){
                     return(private$.lhs_exp)
                   },
                   #' @description get the rhs expression
                   getRhs =  function(){
                     return(private$.rhs_exp)
                   },
                   #' @description get the operator
                   getOp =  function(){
                     return(private$.op)
                   },
                   #' @description set the operator
                   #' @param op binary operator to be set
                   setOp =  function(binop){
                     assert_choice(binop, .globals$binopTypes)
                     private$.op =binop
                   },
                   #' @description set the lhs expression
                   #' @param e expression to set
                   setLhs =  function(e){
                     assertR6(e, "Expression")
                     private$.lhs_exp = e
                   },
                   #' @description set the rhs expression
                   #' @param e expression to set
                   setRhs = function(e){
                     assertR6(e, "Expression")
                     private$.rhs_exp = e
                   },
                   #' @description return the MiniZinc representation
                   c_str = function(){
                     return(sprintf("(%s %s %s)", private$.lhs_exp$c_str(),
                                    private$.op, private$.rhs_exp$c_str()))
                   },
                   #' @description delete flag for internal use
                   getDeleteFlag = function(){
                     return(private$.delete_flag)
                   },
                   #' @description delete the assignment item
                   delete = function(){
                     private$.delete_flag = TRUE
                     helperDeleteExpression("BinOp")
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
                  .op = NULL,
                  #' @field .delete_flag
                  #' used to delete items
                  .delete_flag = FALSE
                ))


#' @title UnOp 
#' @description
#' Unary operation expression in MiniZinc
#' @export
#' @examples 
#' newUnOp = UnOp$new(args = list(Int$new(5)), op = "-")
#' newUnOp$c_str()
#' newUnOp$setArg(1, Int$new(6))
#' newUnOp$setOp("+")
#' newUnOp$c_str()
UnOp = R6Class("UnOp",
               inherit = Expression,
               public = list(
                 #' @description constructor
                 #' @param args list of expressions
                 #' @param op unary operator
                 initialize = function(args, op){
                   assertList(args, "Expression")
                   private$.args = args
                   assert_choice(op, .globals$unopTypes)
                   private$.op = op
                 },
                 #' @description get the number of arguments
                 nargs = function(){
                   return(length(private$.args))
                 },
                 #' @description get all expression arguments
                 getArgs = function(){
                   return(private$.args)
                 },
                 #' @description set all expression arguments
                 #' @param args argument list to be set
                 setArgs = function(){
                   assertList(args, "Expression")
                   private$.args = args
                 },
                 #' @description get the ith expression argument
                 #' @param i index
                 getArg = function(i){
                   return(private$.args[[i]])
                 },
                 #' @description set the ith expression argument
                 #' @param i index
                 #' @param val value of expression to be set
                 setArg = function(i, val){
                   assertR6(val, "Expression")
                   private$.args[[i]] = val
                 },
                 #' @description get the unary operator
                 getOp = function(){
                   return(private$.op)
                 },
                 #' @description set the unary operator
                 #' @param unop unary operator to be set
                 setOp = function(unop){
                   assert_choice(unop, .globals$unopTypes)
                   private$.op = unop
                 },
                 #' @description return the MiniZinc representation
                 c_str =  function(){
                   uoStr = ''
                   for(i in seq(1, length(private$.args), 1)){
                     uoStr = paste0(uoStr, private$.args[[i]]$c_str())
                     if(i < length(private$.args)){
                       uoStr = paste0(uoStr, ", ")
                     }
                   }
                   return(sprintf("%s(%s)", private$.op, uoStr))
                 },
                 #' @description delete flag for internal use
                 getDeleteFlag = function(){
                   return(private$.delete_flag)
                 },
                 #' @description delete the assignment item
                 delete = function(){
                   private$.delete_flag = TRUE
                   helperDeleteExpression("UnOp")
                 }
               ),
               private = list(
                 #' @field .args
                 #' list of expression arguments
                 .args = NULL,
                 #' @field .op
                 #' operator to be used
                 .op = NULL,
                 #' @field .delete_flag
                 #' used to delete items
                 .delete_flag = FALSE
               ))

#' @title Call
#' @description 
#' Create function calls in MiniZinc
#' @export
#' @examples 
#' newCall = Call$new(fnName = "sum", args = list(Int$new(2), Int$new(5)))
#' newCall$c_str()
Call = R6Class("Call",
               inherit = Expression,
               public = list(
                 #' @description constructor
                 #' @param fnName function name
                 #' @param args the list of expressions
                 initialize =  function(fnName , args){
                   assertString(fnName)
                   private$.id = fnName
                   assert_list(args, "Expression")
                   private$.args = args
                 },
                 #' @description get the function id/string
                 getName =  function(){
                   return(private$.id)
                 },
                 #' @description get the function id/string
                 #' @param name new function name
                 setName =  function(name){
                   assertString(name)
                   private$.id = name
                 },
                 #' @description get the number of arguments
                 nargs = function(){
                   return(length(private$.args))
                 },
                 #' @description get the expression list
                 getArgs = function(){
                   return(private$.args)
                 },
                 #' @description set the expression list
                 #' @param args list of expressions to be set
                 setArgs = function(args){
                   assert_list(args, "Expression")
                   private$.args = args
                 },
                 #' @description get the expression based on index
                 #' @param i index 
                 getArg = function(i){
                   return(private$.args[[i]])
                 },
                 #' @description set argument i
                 #' @param i index
                 #' @param e  expression
                 setArg = function(e, i){
                   assertR6(e, "Expression")
                   private$.args[[i]] = e
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
                 },
                 #' @description delete flag for internal use
                 getDeleteFlag = function(){
                   return(private$.delete_flag)
                 },
                 #' @description delete the assignment item
                 delete = function(){
                   private$.delete_flag = TRUE
                   helperDeleteExpression("Call")
                 }
               ),
               private = list(
                 #' @field .id
                 #' the function id
                 .id = NULL,
                 #' @field .lExp
                 #' list of expressions
                 .args = NULL,
                 #' @field .delete_flag
                 #' used to delete items
                 .delete_flag = FALSE
               ))

#' @title Let
#' @description 
#' Create let expression in MiniZinc
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
                getLets = function(){
                  return(private$.let)
                },
                #' @description  set list of local declarations
                #' @param letList list of declarations to be set
                setLets = function(letList){
                  assertList(letList, "Expression")
                  private$.let = letList
                },
                #' @description  access local declaration i
                #' @param i index of let declaration to be accessed
                getLet = function(i){
                  return(private$.let[[i]])
                },
                #' @description  set list of local declarations
                #' @param let declaration to be set
                setLet = function(let){
                  assertList(let, "Expression")
                  private$.let[[i]] = let
                },
                #' @description get the body
                getBody = function(){
                  return(private$.in)
                },
                #' @description set the body
                #' @param expBody expression to be set for body
                setBody = function(expBody){
                  assertR6(expBody, "Expression")
                  private$.in = expBody
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
                },
                #' @description delete flag for internal use
                getDeleteFlag = function(){
                  return(private$.delete_flag)
                },
                #' @description delete the assignment item
                delete = function(){
                  private$.delete_flag = TRUE
                  helperDeleteExpression("Let")
                }
              ),
              private = list(
                #' @field .decl
                #' list of local declarations
                .decl = NULL,
                #' @field .in
                #' body of the let
                .in = NULL,
                #' @field .delete_flag
                #' used to delete items
                .delete_flag = FALSE
              ))

#' @title Ite 
#' @description 
#' Create if-then-else expressions in MiniZinc
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
                #' @description get the if expression list
                getIfs = function(){
                  return(private$.ifs)
                },
                #' @description get the then expression list
                getThens = function(){
                  return(private$.thens)
                },
                #' @description set the if and then expression list
                #' @param ifs expression list to be set
                #' @param thens expression list to be set
                setIfsThens = function(ifs, thens){
                  assertList(ifs, "Expression")
                  assertList(thens, "Expression")
                  assert_true(length(ifs) == length(thens))
                  private$.ifs = ifs
                  private$.thens = thens
                },
                #' @description get the ith if expression
                #' @param i index
                getIf = function(i){
                  return(private$.ifs[[i]])
                },
                #' @description set the ith if expression
                #' @param i index
                #' @param expIf if expression to be set
                setIf = function(i, expIf){
                  assertR6(expIf, "Expression")
                  private$.ifs[[i]] = expIf
                },
                #' @description get the ith then expression
                #' @param i index
                getThen = function(i){
                  return(private$.thens[[i]])
                },
                #' @description set the ith then expression
                #' @param i index
                #' @param expThen then expression to be set
                setThen = function(i, expThen){
                  assertR6(expThen, "Expression")
                  private$.thens[[i]] = expThen
                },
                #' @description get the else expression
                getElse = function(){
                  return(private$.else)
                },
                #' @description get the else expression
                #' @param expElse else expression to be set
                setElse = function(expElse){
                  assertR6(expElse, "Expression")
                  private$.else = expElse
                },
                #' @description get the MiniZinc representation
                c_str = function(){
                  ifthenStr = ''
                  for(i in seq(1, length(private$.ifs), 1)){
                    ifthenStr = paste0(ifthenStr, sprintf("if (%s) then (%s) else", private$.ifs[[i]]$c_str(),
                                        private$.thens[[i]]$c_str()))
                  }
                  return(sprintf("%s (%s) endif", ifthenStr, private$.else$c_str()))
                },
                #' @description delete flag for internal use
                getDeleteFlag = function(){
                  return(private$.delete_flag)
                },
                #' @description delete the assignment item
                delete = function(){
                  private$.delete_flag = TRUE
                  helperDeleteExpression("Ite")
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
                .else = NULL,
                #' @field .delete_flag
                #' used to delete items
                .delete_flag = FALSE
              ))

#' @title VarDecl
#' @description 
#' Contains different fields to create a variable declaration
#' @export
#' @examples 
#' newVarDecl = VarDecl$new(name = "n", 
#' type_inst = TypeInst$new(Type$new(base_type = "int", kind = "par")))
#' newVarDecl$c_str()
VarDecl = R6Class("VarDecl",
                  inherit = Expression,
                  public = list(
                    #' @description constructor
                    #' @param type_inst type instantiation of the variable
                    #' @param name the identifier/name
                    #' @param value value of variable, NULL by default
                    initialize = function(name, type_inst, value = NULL){
                      assertR6(type_inst, "TypeInst")
                      private$.ti = type_inst
                      if(testR6(value, "Expression")){
                        private$.e  =  value
                      }
                      assert_string(name)
                      private$.id = Id$new(name)
                    },
                    #' @description get the identifier object
                    getId = function(){
                      return(private$.id)
                    },
                    #' @description set the identifier object name
                    #' @param name name to be set
                    setId = function(name){
                      assertCharacter(name)
                      private$.id = Id$new(name)
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
                    #' @description overwrite the existing domain
                    #' @param dom domain expression to be set
                    setDomain = function(dom){
                      private$.ti$setDomain(dom)
                    },
                    #' @description get the variable domain
                    getDomain = function(){
                      return(private$.ti$getDomain())
                    },
                    #' @description get the value
                    getValue = function(){
                      return(private$.e)
                    },
                    #' @description set the value
                    #' @param val expression to be set
                    setValue = function(val){
                      assertTRUE(testR6(val, "Expression") ||
                                   testNull(val))
                      private$.e = val
                    },
                    #' @description get the type-inst of the variable declaration
                    ti = function(){
                      return(private$.ti)
                    }, 
                    #' @description get the domain of the variable
                    #' @description return string representation of MiniZinc
                    c_str = function(){
                      retStr = ""
                      var = ""
                      if (self$isVar()){
                        var = "var "  
                      }
                      if(private$.ti$type()$bt() == "unknown"){
                        # unknown base type -- has a domain
                        if(private$.ti$type()$ndim() == 0 && !private$.ti$type()$isSet()){
                          retStr = sprintf("%s%s: %s", var, private$.ti$getDomain()$c_str(), private$.id$getName()) 
                        }else if(private$.ti$type()$isSet()){
                          retStr = sprintf("%s set of %s: %s",var, private$.ti$getDomain()$c_str(),
                                           private$.id$getName())
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
                          # print(indices)
                          retStr = sprintf("array[%s] of %s%s: %s", indices, var, 
                                           private$.ti$getDomain()$c_str(), private$.id$getName())
                        } 
                      }else{
                        # base type known -- domain doesn't exist
                        if(private$.ti$type()$ndim() == 0 && !private$.ti$type()$isSet()){
                          retStr = sprintf("%s%s: %s", var, private$.ti$type()$bt(), private$.id$getName()) 
                        }else if(private$.ti$type()$isSet()){
                          retStr = sprintf("set of %s%s: %s", var, private$.ti$type()$bt(),
                                          private$.id$getName())
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
                          retStr = sprintf("array[%s] of %s%s: %s", indices,
                                           var, bt, private$.id$getName())
                        } 
                      }
                      
                      if(!is.null(private$.e)){
                        return(sprintf("%s = %s", retStr, private$.e$c_str()))
                      }
                      return(sprintf("%s", retStr))
                    },
                    #' @description delete flag for internal use
                    getDeleteFlag = function(){
                      return(private$.delete_flag)
                    },
                    #' @description delete the assignment item
                    delete = function(){
                      private$.delete_flag = TRUE
                      helperDeleteExpression("VarDecl")
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
                    .e = NULL,
                    #' @field .delete_flag
                    #' used to delete items
                    .delete_flag = FALSE
                  ))


#' @title TypeInst 
#' @description 
#' Create type instantiation with indices, etc.
#' @export
#' @examples 
#' TypeInst$new(type = Type$new(base_type = "int", kind = "par" ,dim = 1), 
#'              domain = Set$new(IntSetVal$new(2,5)))
TypeInst = R6Class("TypeInst",
                   inherit = Expression,
                   public = list(
                     #' @description constuctor
                     #' @param type type of declaration
                     #' @param indexExprVec expression list of indices
                     #' @param domain the domain of decision variables
                     initialize = function(type, indexExprVec = NULL, domain = NULL){
                       assertR6(type, "Type")
                       private$.type = type 
                       assertTRUE(length(indexExprVec) == type$ndim() ||
                                    testNull(indexExprVec))
                       if(!testNull(indexExprVec)){
                         for (i in seq(1, length(indexExprVec), 1)) {
                           assertTRUE(testR6(indexExprVec[[i]], "Expression") || 
                                        indexExprVec[[i]] == "int") 
                         } 
                       }
                       private$.indExpr = indexExprVec
                       assertTRUE(testR6(domain, "Expression") || testNull(domain))
                       private$.domain = domain
                     },
                     #' @description get the variable domain
                     getDomain = function(){
                       return(private$.domain)
                     }, 
                     #' @description set the variable domain
                     #' @param dom domain expression to be set
                     setDomain = function(dom){
                       assertR6(dom,"Expression")
                       private$.domain = dom
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
#' @description 
#' Create Annotations in MiniZinc
#' @export
Annotation = R6Class("Annotation",
                     public = list(
                       #' @description constructor
                       #' @param expVec vector of MiniZinc expressions
                       initialize = function(expVec){
                        assertList(expVec, "Expression")
                         private$.expVec = expVec
                       },
                       #' @description get the list of expressions
                       getExps = function(){
                         return(private$.expVec)
                       },
                       #' @description set the list of expressions
                       #' @param expVec list of expressions to be set
                       setExps = function(expVec){
                         assertList(expVec, "Expression")
                         private$.expVec = expVec
                       },
                       #' @description get the MiniZinc expression
                       c_str = function(){
                         retStr = ""
                         for (i in seq(1, length(private$.expVec), 1)) {
                           retStr = paste0(retStr, private$.expVec[[i]]$c_str())
                         }
                         return(sprintf(" :: %s", retStr))
                       },
                       #' @description delete flag for internal use
                       getDeleteFlag = function(){
                         return(private$.delete_flag)
                       },
                       #' @description delete the assignment item
                       delete = function(){
                         private$.delete_flag = TRUE
                         helperDeleteExpression("Annotation")
                       }
                     ),
                     private = list(
                       #' @field .expVec
                       #' list of expressions
                       .expVec = NULL,
                       #' @field .delete_flag
                       #' used to delete items
                       .delete_flag = FALSE
                     ))
