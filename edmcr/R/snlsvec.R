snlsvec <- function(X){
  
  temp <- matrix(rep(TRUE,ncol(X)*nrow(X)),nrow=nrow(X))
  Utri <- temp[upper.tri(temp,diag=TRUE)]
  Ustri <- temp[upper.tri(temp,diag=FALSE)]
  
  x <- X
  x[Ustri] <- sqrt(2) * x[Ustri]
  x <- x[Utri]
  
  return(x)
}