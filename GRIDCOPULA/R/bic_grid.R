#' @title Calculates the BIC of a grid type copula
#' @description This function receives a grid type copula as a parameter and returns the value of the BIC.
#' @return Returns a number with the BIC of a grid type copula.
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
#' bic.grid(copula.grid)
#' @export



bic.grid <- function(gc) {
	mg<- gc
  ll <- ll.grid(x=as.vector(mg$Density), y=as.vector(mg$Quantity))
  value <- -2*ll + (mg$m-1)*(mg$k-1)*log(sum(mg$Quantity))
  return(value)
}

