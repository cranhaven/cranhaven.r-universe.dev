#' @title Evaluates the distribution function of a grid type copula
#' @description Returns the corresponding distribution function values.
#' @return Returns a vector with the corresponding distribution.
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
#' p.grid(df,gc=copula.grid)
#' @export



p.grid <- function(U, V=NULL, gc) {
  mg <- gc
  if(!is.null(V)) {
    u.values <- cbind(U, V)
  } else {
    u.values <- U
  }
  
  k <- mg$k
  m <- mg$m
  volume.matrix <- 0*mg$Density
  volume.matrix[k, ] <- cumsum(mg$Density[k,])
  volume.matrix[k:1, 1] <- cumsum(mg$Density[k:1,1])
  for(i in (k-1):1) {
    for(j in 2:m) {
      volume.matrix[i, j] <- sum(mg$Density[i:k,1:j])
    }
  }
  volume.matrix <- volume.matrix / (k*m)
  
  if(is.null(nrow(u.values))) {
    value <- cdf.grid(u.values, mg= gc, Vm=volume.matrix)
  } else {
    value <- apply(u.values, 1, cdf.grid, mg= gc, Vm=volume.matrix)
  }
  
  return(value)
}

