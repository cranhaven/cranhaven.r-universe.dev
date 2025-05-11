confband_radii <- function(N, Sigma_hat, df, conf.level, n, matsqrt_tol){
  rho_hat <- cov2cor(Sigma_hat)
  t <- rmvt(n = N, SqrtSigma = matsqrt(rho_hat*(df - 2)/df, tol = matsqrt_tol),
            df = df)
  t_max <- apply(t, MARGIN = 1, FUN = function(x) max(abs(x)))
  t_alpha_rho_hat <- quantile(t_max, probs = conf.level)
  radii <- t_alpha_rho_hat*sqrt(diag(Sigma_hat)/n)
  return(radii)
}
