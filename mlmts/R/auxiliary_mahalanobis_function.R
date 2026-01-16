

auxiliary_mahalanobis_function <- function(X_1, X_2) {

  mean_1 <- colMeans(X_1)
  mean_2 <- colMeans(X_2)
  diff_means <- mean_2 - mean_1
  cov_1 <- stats::cov(X_1)
  pseudoinverse_cov_1 <- pracma::pinv(cov_1)
  phi <- t(diff_means) %*% pseudoinverse_cov_1 %*% diff_means
  measure_normal <- 2 * (1 - stats::pnorm(phi))
  return(1 - measure_normal)

}

auxiliary_mahalanobis_function_extra <- function(X_1, X_2) {

  mean_1 <- colMeans(X_1)
  mean_2 <- colMeans(X_2)
  diff_means <- mean_2 - mean_1
  cov_1 <- stats::cov(X_1)
  cov_2 <- stats::cov(X_2)
  pseudoinverse_cov_1 <- pracma::pinv(cov_1)
  pseudoinverse_cov_2 <- pracma::pinv(cov_2)
  dis_mah_1 <- sqrt(t(diff_means) %*% pseudoinverse_cov_1 %*% diff_means)
  dis_mah_2 <- sqrt(t(diff_means) %*% pseudoinverse_cov_2 %*% diff_means)
  return(0.5*(dis_mah_1 + dis_mah_2))

}



