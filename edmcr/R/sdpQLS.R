sdpQLS <- function(A,b){
  
  m <- nrow(A)
  n <- ncol(A)
  
  #Minimum Degree Ordering
  
  q_colamd <- colamdR(A)
  A <- A[,q_colamd]
  
  ###### sqr #######################
  #QRDecomp <- qr(A)
  
  #R <- qr.R(QRDecomp)
  #Q <- qr.Q(QRDecomp)
  
  #qI <- sort.list(QRDecomp@q) # the inverse 'q' permutation
  ################################
  
  #Permute the original permutation
  #Pc <- q_colamd[qI]
  #c <- t(Q) %*% b
  #X <- solve(R, c)
  QRDecomp <- qr(A)
  X <- qr.coef(QRDecomp,b)
  X[q_colamd] <- X
  
  return(X)
}