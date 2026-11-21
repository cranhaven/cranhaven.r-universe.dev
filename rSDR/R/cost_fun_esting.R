cost_fun_esting<-function(C_value,X_train,X_test,Y_test,alpha){
  # Pairwise euclidean distance nxn for response Y
  PDY <- as.matrix(stats::dist(Y_test, method = "euclidean"))
  #update the power of euclidean distance
  KY <- PDY^alpha  # Assuming alpha = 1 for simplicity
  p <-  ncol(X_test)
  n <- length(Y_test)
  # FIX: double centering via H K H
  H <- diag(n) - matrix(1, n, n) / n
  B<-H%*%KY%*%H

  # X covariance and its matrix sqrt and inverse sqrt, with ridge for stability
 #We use Covariance from training
   N <- stats::cov(X_train) #pxp
  eig <- eigen(N, symmetric = TRUE)
  lam <- pmax(eig$values, 1e-10)
  Nhalf     <- eig$vectors %*% (diag(sqrt(lam), p, p)) %*% t(eig$vectors)
  Nhalf_inv <- eig$vectors %*% (diag(1 / sqrt(lam), p, p)) %*% t(eig$vectors)

  # Standardize X
  Z <- X_test %*% Nhalf_inv   # n x p
  ZC <- Z %*% C_value
  PDZC <- as.matrix(stats::dist(ZC))
  f <- -mean((PDZC^alpha) * B)
  f
}
