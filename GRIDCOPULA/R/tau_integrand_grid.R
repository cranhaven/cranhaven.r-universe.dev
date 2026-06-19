#' @title Evaluates the integrand function for the Kendall's tau concordance measure of a grid type copula.
#' @description Returns the corresponding value of the integrand function.
#' @param x a vector with values of the U1 variable.
#' @param y a vector with values of the U2 variable.
#' @param gc a grid type copula object.
#' @examples



tau.integrand.grid <- function(x, y, gc) {
	mg<- gc
  value <- p.grid(U=as.numeric(x), V=as.numeric(y), gc = mg) * d.grid(U=as.numeric(x), V=as.numeric(y), gc=mg)
  return(value)
}


