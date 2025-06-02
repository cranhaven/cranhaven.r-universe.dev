#' @title Calculates the multiplication of normal densities
#' @description Returns a vector with the values of the product of densities.
#' @param x a vector with the values of the variable x.
#' @param y a vector with the values of the variable y.
#' @param parameters a vector with the values of the mean and the standard
#' deviation
#' @examples


normal.multiplication <- function(x, y, parameters=c(0,0,1,1)) {
  f.x <- dnorm(x=x, mean=parameters[1], sd=parameters[3])
  f.y <- dnorm(x=y, mean=parameters[2], sd=parameters[4])
  value <- f.x * f.y
  return(value)
}
