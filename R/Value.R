#' @title SetVal class
#' 
#' @description possible types of set values in MiniZinc
#' 
#' @import R6
#' @import checkmate
#' 
#' @export
SetVal = R6Class("SetVal",
                 public = list(
                  #' @description  constructor
                  #' @param val
                  initialize = function(val){
                    if(all(names(val) == c("l","u"))){
                      # int set val
                      assert_true((testR6(val[['l']]) || test_numeric(val[['l']])) &&
                                    testR6(val[['u']]) || test_numeric(val[['u']]))
                      
                      if(test_numeric(val[['l']]) && test_numeric(val[['u']])){
                        assert(val[['l']] - floor(val[['l']]) == 0 && val[['u']] - floor(val[['u']]) == 0)
                        private$.isv = val
                      }else if(testR6(val[['l']], "VarDecl") && test_numeric(val[['u']])){
                        assert_true(val[['l']]$ti()$type()$ndim() == 0 &&
                                      val[['l']]$ti()$type()$bt() == "INT")
                        private$.isv = c( l = val[['l']]$id(), u = val[['u']])
                        assert(val[['u']] - floor(val[['u']]) == 0)
                      }else if(test_numeric(val[['l']]) && testR6(val[['u']], "VarDecl")){
                        assert(val[['l']] - floor(val[['l']]) == 0)
                        assert_true(val[['u']]$ti()$type()$ndim() == 0 &&
                                      val[['u']]$ti()$type()$bt() == "INT")
                        private$.isv = c( l = val[['l']], u = val[['u']]$id())
                      }else if(testR6(val[['l']], "VarDecl") && testR6(val[['u']], "VarDecl")){
                        assert_true(val[['u']]$ti()$type()$ndim() == 0 &&
                                      val[['u']]$ti()$type()$bt() == "INT")
                        assert_true(val[['l']]$ti()$type()$ndim() == 0 &&
                                      val[['l']]$ti()$type()$bt() == "INT")
                        private$.isv = c( l = val[['l']]$id(), u = val[['u']]$id())
                      }else{
                        stop("not supported")
                      }
                      
                    }
                  },
                  #' @description return the int set value
                  isv = function(){
                    return(private$.isv)
                  },
                  #' @description return the float set value
                  fsv = function(){
                    return(private$.fsv)
                  }
                 ),
                 private = list(
                   #' @field .isv
                   #' the int set value
                   .isv = NULL,
                   #' @field .fsv
                   #' the float set value
                   .fsv = NULL
                 ))

#' @title IntVal class
#' 
#' @description create an Integer Value in MiniZinc
#' 
#' @import R6
#' @import checkmate
#' @export
IntVal =  R6Class("IntVal",
                  public = list(
                    #' @description constructor
                    #' @param val
                    initialize = function(val){
                      assert_numeric(val)
                      assert_true(val - floor(val) == 0)
                      private$.val = val
                    },
                    #' @description return the value
                    v = function(){
                      return(private$.val)
                    }
                  ),
                  private = list(
                    #' @field .val
                    #' the integer value
                    .val = NULL
                  ))

#' @title FloatVal class
#' 
#' @description create a Float Value in MiniZinc
#' 
#' @import R6
#' @import checkmate
FloatVal = R6Class("FloatVal",
                   public = list(
                     #' @description constructor
                     #' @param val float literal val
                     initialize = function(val){
                       assert_numeric(val)
                       private$.val = val
                     },
                     #' @description return the value
                     v = function(){
                       return(private$.val)
                     }
                   ),
                   private = list(
                     #' @field .val
                     #' the integer value
                     .val = NULL
                   ))