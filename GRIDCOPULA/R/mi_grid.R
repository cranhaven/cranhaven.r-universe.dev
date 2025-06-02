#' @title Calculates the mutual information of a grid type copula
#' @return Returns a number with the mutual information.
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
#' copula.ml <- estimate.gridCopula(U = df, k = k, m = m , method = "ml")
#' copula.ls <- estimate.gridCopula(U=df, k=k, m=m , method = "ls")
#' mi.grid(gc = copula.ml)
#' mi.grid(gc = copula.ls)
#' @export


mi.grid <- function(gc) {
	mg<- gc
  k <- mg$k
  m <- mg$m
  value <- 0
  for(i in 1:k) {
    for(j in 1:m) {
      if( 0<mg$Density[i,j] ) {
        value <- value + ( mg$Density[i,j] * log(mg$Density[i,j]) )
      } else {
        if( mg$Density[i,j]==0 ) {
          value <- value + 0
        } else {
          value <- value -Inf
        }
      }
    }
  }
  value <- value / (k*m)
  return(value)
}

