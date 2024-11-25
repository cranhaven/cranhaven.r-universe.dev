snlsMat <- function(x){
  
  tn <- max(nrow(X),ncol(X))
  n <- floor(sqrt(2*tn))
  E <- matrix(rep(TRUE,n^2),nrow=n)
  indsu <- E[upper.tri(E,diag=TRUE)]
  indsuu <- E[upper.tri(E,diag=FALSE)]
  X <- matrix(rep(0,n^2))
  X[indsu] <- x
  v <- diag(X)
  diag(X) <- 0
  X[indsuu] <- X[indsuu]/sqrt(2)
  X <- X + t(X)
  X <- X+diag(v)
  return(X)
}