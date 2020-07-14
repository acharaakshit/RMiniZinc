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
                      if(val[['l']] - floor(val[['l']]) == 0 && val[['u']] - floor(val[['u']]) == 0){
                        # int set val
                        private$.isv = val
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