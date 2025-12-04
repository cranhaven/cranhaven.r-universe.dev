delta.est = function(S.hat.A, Omega.hat0, lam1){
  # delta.est: the function estimating divergence matrix (Delta_m) of the mode
  #            corresponding to S.hat.A.
  pm = dim(S.hat.A)[1]
  B.hat = Omega.hat0 %*% S.hat.A - diag(pm)
  Z = array(rep(0, 2*pm^2), dim=c(pm,pm,2))
  Z[,,2] = abs(B.hat) - lam1
  delta.hat = sign(B.hat) * apply(Z, 1:2, max)
  return(delta.hat)
}
