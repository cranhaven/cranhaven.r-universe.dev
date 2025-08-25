#' @title Escape Function in Case of Errors
#' 
#' @description 
#' Evaluate condition and return error message if condition not satisfied. 
#' 
#' @param condition a logical expresssion (e.g. is.null(test_variable))
#' @param msg the error message returned if the condition is not met.
#' 
#' @examples 
#' assert_function(1==2,"Incorrect inequality")
#' 
#' @returns No return value, called for side effects.
#' @export

assert_function <- function(condition,msg){
  if(condition) {
    stop(msg)
  }
}