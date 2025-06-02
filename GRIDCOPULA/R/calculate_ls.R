#' @title Creates a grid type copula according to a given data set and the Pfeifer proposal
#' @description Returns a list with a matrix with the density over the grid,
#' a matrix with the quantity of data over the grid, the number of subintervals for the U2 variable,
#' the number of subintervals for the U1 variable.
#' @param U a matrix of size nx2 with the observed values.
#' @param k positive integer indicating the number of subintervals for the U2 variable.
#' @param m positive integer indicating the number of subintervals for the U1 variable.
#' @examples





calculate.ls<- function(U, k, m) {
  Qm <- count.grid(U, k, m)
  Am <- (m*k) * (Qm / sum(Qm))

  # limSolve package
  Quad.m <- diag(k*m)
  Quad.v <- as.vector(Am)
  Eq.m <- matrix(0, nrow=k+m, ncol=k*m)
  for(i in 1:m) {
    Eq.m[i, ((i-1)*k+1):(i*k)] <- 1
  }
  for(i in 1:k) {
    Eq.m[(m+i), seq(i, m*k, by=k)] <- 1
  }
  Eq.v <- c(rep(k, m), rep(m, k))
  Eq.m <- Eq.m[-nrow(Eq.m), ]
  Eq.v <- Eq.v[-length(Eq.v)]
  Ineq.m <- rbind(diag(k*m), -diag(k*m))
  Ineq.v <- c(rep(0,k*m), -rep(min(k,m),k*m))
  value <- limSolve::lsei(A=Quad.m, B=Quad.v, E=Eq.m, F=Eq.v, G=Ineq.m, H=Ineq.v, 
                type=1)
  Dm <- matrix(value$X, nrow=k, ncol=m)
  
  return(list(Density=Dm, Quantity=Qm, m=m, k=k))
}


