#' @title Evaluates the density of a grid type copula
#' @description Returns the corresponding density values of a grid type copula.
#' @return Returns a vector with the corresponding density.
#' @param U a matrix of size nx2 with the observed values. It can also be a vector of size kx1 with the values of the U1 variable.
#' @param V optional, a vector of size kx1 with the values of the U2 variable.
#' @param gc a grid type copula object.
#' @examples
#' n <- 500
#' x <- rgamma(n,4,1/2)
#' e <- rnorm(n,0,.3)
#' y <- sin(x+e)
#' Fx <- ecdf(x)
#' Fy <- ecdf(y)
#' u <- Fx(x)
#' v <- Fy(y)
#' df <- cbind(u,v)
#' k <- 5
#' m <- 4
#' copula.grid <- estimate.gridCopula(U = df, k = k, m = m , method = "ml")
#' d.grid(df,gc=copula.grid)
#' @export


d.grid <- function(U, V=NULL, gc) {
	mg<- gc 
  if(!is.null(V)) {
    u.values <- cbind(U, V)
  } else {
    u.values <- U
  }
  
  if(is.null(nrow(u.values))) {
    value <- pdf.grid(U, mg=mg)
  } else {
    value <- apply(u.values, 1, pdf.grid, mg=mg)
  }
  
  return(value)
}
