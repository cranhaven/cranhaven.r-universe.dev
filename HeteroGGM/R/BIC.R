BIC <- function(data, mu_hat, Theta_hat, L.mat){

  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## The name of the function: BIC
  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## Description:
  ##            Calculating the adaptive BIC-type criterion.
  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## Required preceding functions or packages:
  ##            R functions: f.den.vec()
  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## Input:
  ## @ data: n * p matrix, the design matrix.
  ## @ mu_hat: K0_hat * p matrix, the estimated mean vectors of K0_hat subgroups.
  ## @ Theta_hat: p * p * K0_hat array, the estimated precision matrices of K0_hat subgroups.
  ## @ L.mat: n * K0_hat matrix, the estimated probability that each sample belongs to each subgroup.
  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## Output:
  ## A list P including:
  ## @ fit.error: a float value, the value of the loss function (without penalty function).
  ## @ df: a float value, the penalty value for non-zero parameters corresponding the choice of given tuning parameters.
  ## @ bic: a float value, the BIC value corresponding the choice of given tuning parameters.
  ## ------------------------------------------------------------------------------------------------------------------------------------------

  n = nrow(data)
  K = nrow(mu_hat)

  # fitting error
  pi_vec = apply(L.mat, 2, sum)/n
  fit.error_mat = matrix(0, n, K)
  for(k in 1:K) {
    fit.error_mat[,k] = pi_vec[k] * f.den.vec( data, as.numeric(mu_hat[k,]), Theta_hat[,,k] )
  }
  fit0 = apply(fit.error_mat, 1, sum)
  fit.error = sum(log( fit0 + min(fit0[fit0>0]) ))
  fit.error = - 2*fit.error

  # degrees of freedom
  for(i in 1:K){
    Theta_hat[upper.tri(Theta_hat[, , i], diag = T)] = 0
  }

  df =  log(n) * length(which(mu_hat != 0)) + 2 * length(which(Theta_hat != 0))
  bic = fit.error + df
  P = list()
  P$fit.error = fit.error
  P$df = df
  P$bic = bic
  return(P)
}
