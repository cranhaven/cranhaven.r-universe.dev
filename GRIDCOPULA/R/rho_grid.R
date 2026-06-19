#' @title Calculates the Spearman's rho concordance measure for a grid type copula
#' @return Returns a number with the corresponding value.
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
#' k <- 10
#' m <- 10
#' copula.grid <- estimate.gridCopula(U = df, k = k, m = m , method = "ml")
#' rho.grid(gc = copula.grid)
#' @export

rho.grid <- function(gc) {
	k <- gc$k
	m <- gc$m
	u1 <- seq((1/m)/2,1,1/m)
	u2 <- seq((1/k)/2,1,1/k)
	C <- outer(u1,u2,p.grid, gc = gc)
	value <- 12 * (sum((1 /(m * k)) * C) - 1/4)
	return(value)
}
