#' @title Generates a random sample from a grid type copula
#' @return Returns a  matrix of size nx2 with the random sample.
#' @param n an integer number indicating the size of the sample.
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
#' k <- 15
#' m <- 15
#' copula.grid <- estimate.gridCopula(U = df, k = k, m = m , method = "ml")
#' df2 <- r.grid(n = n, gc = copula.grid)
#' data.grid(df, k=k, m=m)
#' data.grid(df2, k=k, m=m)
#' @export


r.grid <- function(n, gc) {
	mg<- gc
  U <- runif(n)
  V <- r.cond.grid(U, mg)
  return(cbind(U,V))
}
