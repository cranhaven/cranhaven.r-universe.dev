Thetaest.admm = function(S.hat.A, deltaI, lam2, Omega.hat0,
                         max_iter=10, eps=1e-3, kappa = 1){
  # Thetaest.cd: the function estimating transfer learning-based estimator of
  #              precision matrix of the mode corresponding to S.hat.A, via
  #              ADMM algorithm.
  p = dim(S.hat.A)[1]
  SI = S.hat.A+kappa*diag(p)
  Theta_hat = Omega.hat0

  for (j in 1:p){
    thetaj = Omega.hat0[,j]
    v = thetaj
    ej = rep(1,p)
    ej[j] = 0
    gamma = rep(0,p)
    iter = 0
    diff = 10
    while(iter < max_iter && diff > eps){
      thetaj0 = thetaj
      thetaj = as.numeric(solve(SI) %*% ( deltaI[,j] + gamma + kappa*v ))
      v = S_soft.vec(thetaj - gamma/kappa, lam2, ej)
      gamma = gamma + kappa * ( v - thetaj )

      diff = sqrt( sum((thetaj - thetaj0)^2) / p )
      iter = iter + 1
    }
    Theta_hat[,j] = v
  }
  return(Theta_hat)
}
