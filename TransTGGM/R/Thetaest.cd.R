Thetaest.cd = function(S.hat.A, deltaI, lam2, Omega.hat0, max_iter=10, eps=0.001){
  # Thetaest.cd: the function estimating transfer learning-based estimator of
  #              precision matrix of the mode corresponding to S.hat.A, via
  #              coordinate descent algorithm.
  p = dim(S.hat.A)[1]
  Theta_hat = Omega.hat0
  for (j in 1:p){
    thetaj = Omega.hat0[,j]
    iter = 0
    diff = 10
    while(iter < max_iter && diff > eps){
      thetaj0 = thetaj
      for (i in 1:p){
        Sj = S.hat.A[i,]
        thetaji = deltaI[i,j] - Sj %*% thetaj + Sj[i] * thetaj[i]
        if(i == j){
          thetaj[i] = S_soft(thetaji, 0) / Sj[i]
        }else{
          thetaj[i] = S_soft(thetaji, lam2) / Sj[i]
        }
      }
      diff = sqrt( sum((thetaj - thetaj0)^2) / p )
      iter = iter + 1
    }
    Theta_hat[,j] = thetaj
  }
  return(Theta_hat)
}
