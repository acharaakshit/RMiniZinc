#' @title Integer set value
#' 
#' @description integer range set value in MiniZinc
#' 
#' @import R6
#' @import checkmate
#' @export
IntSetVal = R6Class("IntSetVal",
                    public = list(
                      #' @description constructor
                      #' @param imin minimum value of type IntVal
                      #' @param imax maximum value of type IntVal
                      initialize = function(imin, imax){
                        assertR6(lhs, "IntVal")
                        private$.min  = imin
                        assertR6(rhs, "IntVal")
                        private$.max = imax
                        assert_true(imax$v() >= imin$v())
                      },
                      #' @description get the minimum IntVal
                      getMin = function(){
                        return(private$.min)
                      },
                      #' @description set the minimum IntVal
                      #' @param val IntVal value to be set
                      setMin = function(val){
                        assertR6(val, "IntVal")
                        private$.min = val
                      },
                      #' @description get the maximum IntVal
                      getMax = function(){
                        return(private$.max)
                      },
                      #' @description set the maximum IntVal
                      #' @param val IntVal value to be set
                      setMax = function(val){
                        assertR6(val, "IntVal")
                        private$.max = val
                      }
                    ),
                    private = list(
                      #' @field .min
                      #' minimum value of integer range
                      .min = NULL,
                      #' @field .max
                      #' maximum value of integer range
                      .max = NULL
                    ))


#' @title Float set value
#' 
#' @description float set range in MiniZinc
#' 
#' @import R6
#' @import checkmate
#' @export
FloatSetVal = R6Class("FloatSetVal",
                      public = list(
                        #' @description constructor
                        #' @param fmin the minimum FloatVal
                        #' @param fmax the maximum FloatVal
                        initialize = function(fmin, fmax){
                          assertR6(fmin, "FloatVal")
                          assertR6(fmax, "FloatVal")
                          assert_true(fmax$v() >= fmin$v())
                          private$.min = fmin
                          private$.max = fmax
                        },
                        #' @description get the minimum FloatVal
                        getMin = function(){
                          return(private$.min)
                        },
                        #' @description set the minimum FloatVal
                        #' @param val FloatVal value to be set
                        setMin = function(val){
                          assertR6(val, "FloatVal")
                          private$.min = val
                        },
                        #' @description get the maximum FloatVal
                        getMax = function(){
                          return(private$.max)
                        },
                        #' @description set the maximum FloatVal
                        #' @param val FloatVal value to be set
                        setMax = function(val){
                          assertR6(val, "FloatVal")
                          private$.max = val
                        }
                      ),
                      private = list(
                        #' @field .min
                        #' minimum FloatVal
                        .min = NULL,
                        #' @field .max
                        #' maximum FloatVal
                        .max = NULL
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
                    #' @param val int value to be assigned
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
#' 
#' @export
FloatVal = R6Class("FloatVal",
                   public = list(
                     #' @description constructor
                     #' @param val float value to be assigned
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