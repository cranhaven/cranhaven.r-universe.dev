################################################################################
#
# Simulate from a truncated multivariate for transformed coefficients
#
################################################################################

simul_tmvnorm <- function(object, B = 100)
{
  #----- Checks
  # Constraint matrix
  bvec <- object$bvec
  Cmat <- object$Cmat
  dims <- dim(Cmat)
  if (dims[1] > dims[2]) {
    stop(paste0("More constraints than parameters, ",
      "impossible to simulate from truncated normal. ",
      "Consider bootstrap inference instead."))
  } else if (dims[1] < dims[2]){
    # Augment constraint matrix to be squared
    Hmat <- nullspace(t(Cmat))
    Cmat <- rbind(Cmat, t(Hmat))
    bvec <- c(bvec, rep(-Inf, ncol(Hmat)))
  }
  
  #----- Covariance matrix of alphas

  vcovmat <- vcov_alpha(object)
  
  #----- Transform on tmvn scale

  # Covariance matrix
  Tmat <- Cmat %*% vcovmat %*% t(Cmat)
  
  # Constraint bounds
  coefs <- unlist(object$alpha)
  Tvec <- bvec - Cmat %*% coefs
  
  #----- Simulate
  # From truncated multivariate normal
  mvtsimu <- TruncatedNormal::rtmvnorm(n = B,
    mu = rep(0, nrow(Cmat)), sigma = Tmat, lb = Tvec)
  
  # Backtransform simulations
  backsimu <- coefs + solve(Cmat) %*% t(mvtsimu)
  
  #----- Return
  t(backsimu)
}