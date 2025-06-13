eigenkronecker <- function(x, n) {
  e <- eigen(x, symmetric=TRUE, only.values=FALSE)
  d <- e$values
  v <- e$vectors
  D <- rep(d, each=n)
  V <- kronecker(v, diag(n))
  list(values=D, vectors=V)  
}