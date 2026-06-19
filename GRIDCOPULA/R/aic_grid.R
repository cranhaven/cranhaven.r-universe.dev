#' @title Calculates the Akaike Information Criterion "AIC" of a grid type copula
#' @description This function receives a grid type copula as a parameter and returns the value of the AIC.
#' @return Returns a number with the AIC of a grid type copula.
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
#' aic.grid(copula.grid)
#' @export



aic.grid <- function(gc) {
  mg<- gc
  ll <- ll.grid(x=as.vector(mg$Density), y=as.vector(mg$Quantity))
  value <- -2*ll + (mg$m-1)*(mg$k-1)
  return(value)
}
