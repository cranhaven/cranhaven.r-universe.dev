#Normal Estimations for use in SDP algorithm

###
sdpNormest <- function(S, tol=1e-6){ 
  
  x <- colSums(abs(S))
  e <- sqrt(sum(x^2))
  x <- x/e
  cnt <- 0
  e0 <- 0
  while(abs(e-e0) > tol*e){
    e0 <- e
    Sx <- S %*% x
    x <- t(S) %*% Sx
    normx <- sqrt(sum(x^2))
    e <- normx/sqrt(sum(Sx^2))
    x <- x/normx
    cnt = cnt+1
  }
  
  return(e)
  
}

###
sdpNormestfro <- function(S){
  X <- colSums(abs(S))
  NormX <- sqrt(sum(X^2))
  return(NormX)
}