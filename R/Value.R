#' @title Integer set value
#' @description integer range set value in MiniZinc
#' @export
IntSetVal = R6Class("IntSetVal",
                    public = list(
                      #' @description constructor
                      #' @param imin minimum int value
                      #' @param imax maximum int value
                      initialize = function(imin, imax){
                        private$.min  = IntVal$new(val = imin) 
                        private$.max  = IntVal$new(val = imax) 
                        assert_true(private$.max$v() >= private$.min$v())
                      },
                      #' @description get the minimum IntVal
                      getMin = function(){
                        return(private$.min$v())
                      },
                      #' @description set the minimum IntVal
                      #' @param val int value to be set
                      setMin = function(val){
                        assertTRUE(val <= private$.max$v())
                        private$.min = IntVal$new(val)
                      },
                      #' @description get the maximum IntVal
                      getMax = function(){
                        return(private$.max$v())
                      },
                      #' @description set the maximum IntVal
                      #' @param val int value to be set
                      setMax = function(val){
                        assertTRUE(val >= private$.min$v())
                        private$.max = IntVal$new(val)
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
#' @description float set range in MiniZinc
#' @export
FloatSetVal = R6Class("FloatSetVal",
                      public = list(
                        #' @description constructor
                        #' @param fmin the minimum FloatVal
                        #' @param fmax the maximum FloatVal
                        initialize = function(fmin, fmax){
                          private$.min = FloatVal$new(fmin)
                          private$.max = FloatVal$new(fmax)
                          assert_true(private$.max$v() >= private$.min$v())
                        },
                        #' @description get the minimum float value
                        getMin = function(){
                          return(private$.min$v())
                        },
                        #' @description set the minimum float value
                        #' @param val float value to be set
                        setMin = function(val){
                          private$.min = FloatVal(val)
                        },
                        #' @description get the maximum float value
                        getMax = function(){
                          return(private$.max$v())
                        },
                        #' @description set the maximum float value
                        #' @param val float value to be set
                        setMax = function(val){
                          private$.max = FloatVal(val)
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

#' @title IntVal class (not exposed to user)
#' @description create an Integer Value in MiniZinc
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

#' @title FloatVal class (not exposed to user)
#' @description create a Float Value in MiniZinc
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