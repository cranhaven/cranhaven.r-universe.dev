#' @title Checking the main parameters
#' \code{link{sel_herb}}

#' @description Is called within \code{sel_herb()} to check for the existence of the necessary data. 

#' @template put
#' @template sdrate
#' @template thresh
#' @template rate


herb_check <- function(put, sdrate, thresh, rate){
cat("Check parameters for sel_herb... ")
if(anyNA(c(put, sdrate, thresh, rate))){stop("To use sel_herb these parameters must be defined: put, sdrate, thresh, rate.")}
cat(" finished!\n")
#return()
}