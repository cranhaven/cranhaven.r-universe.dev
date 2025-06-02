#' @title Generates a conditional random sample from a grid type copula
#' @description Returns a vector of size nx1 with the random sample of the U2 variable.
#' @param U a vector of size nx1 with values of the U1 variable.
#' @param mg a grid type copula object.
#' @examples



r.cond.grid <- function(U, mg) {
  k <- mg$k
  m <- mg$m
  v.breaks <- seq(0, 1, length.out=k+1)
  u.breaks <- seq(0, 1, length.out=m+1)
  V <- 0*U
  for(i in 1:m) {
    if(i < m) {
      u.posicion <- which(u.breaks[i] <= U & U <  u.breaks[i+1])
    } else {
      u.posicion <- which(u.breaks[i] <= U & U <= u.breaks[i+1])
    }
    v.cantidad <- length(u.posicion)
    if( 0 < v.cantidad ) {
      Fv <- c(0, cumsum(mg$Density[k:1, i]))
      Fv <- Fv / Fv[k+1]
      p <- runif(v.cantidad)
      for(j in 1:k) {
        if(j < k) {
          p.posicion <- which(Fv[j] <= p & p <  Fv[j+1])
        } else {
          p.posicion <- which(Fv[j] <= p & p <= Fv[j+1])
        }
        p.cantidad <- length(p.posicion)
        
        if( 0 < p.cantidad ) {
          V[u.posicion[p.posicion]] <- v.breaks[j] + ((v.breaks[j+1]-v.breaks[j])/(Fv[j+1]-Fv[j]))*(p[p.posicion]-Fv[j])
        }
      }
    }
  }
  return(V)
}
