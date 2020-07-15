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
                 #' @param value the value of the integer
                 initialize =  function(value){
                   assertR6(value, "IntVal")
                   private$value = value 
                 },
                 #' @description get the integer value
                 getIntVal = function(){
                   return (private$value)
                 }
               ),
               private = list(
                 #' @field value
                 #' object of class expression
                 value = NULL
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
                 #' @param value the value of the integer
                 initialize =  function(value){
                   assertR6(value, "FloatVal")
                   private$value = value 
                 },
                 #' @description get the integer value
                 getFloatVal = function(){
                   return (private$value)
                 }
               ),
               private = list(
                 #' @field .value
                 #' object of class expression
                 value = NULL
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
                #' @param setVal the set value
                initialize = function(setVal){
                  assertR6(setVal, "SetVal")
                  private$.setVal = setVal 
                },
                #' @description return the value
                v = function(){
                  return(private$.setVal)
                }
              ),
              private = list(
                #' @field .setVal
                #' the value of the set
                .setVal = NULL
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
                   initialize =  function(exprVec){
                     assert_list(exprVec, "Expression")
                     private$value = value 
                   },
                   #' @description get the integer value
                   dims = function(){
                     # only supporting 1 dimensional arrays as of now
                     return (1)
                   }
                 ),
                 private = list(
                   #' @field .value
                   #' object of class expression
                   .value = NULL
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
                        #' @param id the name of element
                        #' @param index the array indices
                        initialize =  function(id, index){
                          assertR6(id, "Id")
                          private$.id = id
                          assertR6(index, "Id")
                          private$.index = index
                        },
                        id = function(){
                          return(private$.id)
                        },
                        index = function(){
                          return(private$.index)
                        }
                      ),
                      private = list(
                        #' @field .id
                        #' the id of array
                        .id = NULL,
                        #' @field .index
                        #' index of the array
                        .index = NULL
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
                       #' @param the name of the iterator
                       initialize = function(IN = NULL, where = NULL, iterator = "i"){
                         assert(testR6(IN, "Expression"),
                                  testR6(where, "Expression"),
                                combine = "or")
                         assert_string(iterator)
                         private$.iter_id = Id$new(iterator)

                         if(!is.null(IN))
                            private$.in = IN
                         else if(!is.null(where))
                           private$where = where
                       },
                       #' @description get the in expression
                       In = function(){
                         return(private$.in)
                       },
                       #' @description get the where expression
                       where = function(){
                         return(private$.where)
                       },
                       #' @description get the iterator id
                       iter_id = function(){
                         return(private$.iter_id)
                       }
                     ),
                     private = list(
                       #' @field .in
                       #' in expression
                       .in = NULL,
                       #' @field where
                       #' where expression
                       .where = NULL,
                       #' @field .iter_id
                       #' the iterator id
                       .iter_id = NULL
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
                           #' @param expression inside the comprehension
                           initialize = function(generators, expression){
                             assert_list(generators, "Generator")
                             private$.generators = generators
                             assertR6(expression, "Expression")
                             private$.expression = expression
                           },
                           #' @description get the number of generators
                           ngens = function(){
                             return(length(private.generators))
                           },
                           #' @description get the ith generator expression
                           #' @param i index
                           gen_i = function(i){
                             return(private$.generators[[i]])
                           },
                           #' @description get the expression
                           e = function(){
                             return(private$.expression)
                           }
                         ),
                         private = list(
                           #' @field .generators
                           #' a vector of generators
                           .generators = NULL,
                           #' @field .expression
                           #' the comprehension expression
                           .expression = NULL
                         ))

#' @title Binop class
#' 
#' @description create a binary operator expression
#' @import R6
#' @import checkmate
#' @export 
Binop = R6Class("Binop",
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

#' @title Id class (not exposed to the user)
#' 
#' @description create a new Id in MiniZinc
#' @import R6
#' @import checkmate
Id  = R6Class("Id",
              inherit = Expression,
              public = list(
                #' @description constructor
                #' @param ID id to be created
                initialize = function(ID){
                  assert_string(ID)
                  private$.id = ID
                },
                #' @description get the string identifier
                id = function(){
                  return(private$.id)
                }
              ),
              private = list(
                #' @field .id
                #' the string identifier
                .id = NULL
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
                       assert_true(testR6(indexExprVec, "Expression") || test_null(indexExprVec))
                       if(testR6(indexExprVec, "Expression")){
                         assert_false(type$isSet())
                       }
                       private$.indExpr = indexExprVec
                       assert_true(testR6(domain, "SetVal") || test_null(domain))
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
                 #' @param lExp the list of expressions
                 initialize =  function(fn_id, lExp){
                   assert_string(fn_id)
                   private$.id = Id$new(fn_id)
                   assert_list(lExp, "Expression")
                   private$.lExp = lExp
                 },
                 #' @description the function id object
                 id =  function(){
                   return(private$.id)
                 },
                 #' @description get the number of arguments
                 nargs = function(){
                   return(private$.nargs)
                 },
                 #' @description get the expression based on index
                 #' @param i index 
                 e_i = function(i){
                   assert_true(!test_null(private$.lExp[[i]]))
                   return(private$.lExp[[i]])
                 }
               ),
               private = list(
                 #' @field .id
                 #' the function id
                 .id = NULL,
                 #' @field .lExp
                 #' list of expressions
                 .lExp = NULL,
                 #' @field .nargs
                 #' number of arguments to the call
                 .nargs = NULL
               ))