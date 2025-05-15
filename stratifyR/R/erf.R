#' To calculate the error for a normal variable
#'
#' This function calculates the value of the error according to the          
#' normally distributed variable using the idea presented in   
#' Abramowitz and Stegun (2011)
#'
#' @param x The data that is provided
#'
#' @return \code{} Gives the error for a normal variable
#'
#' @author Karuna Reddy <karuna.reddy@usp.ac.fj>\cr
#' MGM Khan <khan_mg@usp.ac.fj>
#'
#the 'error function'
erf <- function(x)
  {
  2*pnorm(x*sqrt(2)) - 1
  }
#the 'complementary error function'
erfc <- function(x)
  {
  2*pnorm(x*sqrt(2), lower.tail = FALSE)
  }
#and the inverses
erfinv <- function(x)
  {
  qnorm((1 + x)/2)/sqrt(2)
  }
erfcinv <- function(x)
  {
  qnorm(x/2, lower.tail = FALSE)/sqrt(2)
}
######################################################################