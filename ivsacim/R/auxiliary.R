#' This function is to compute the centralized instrument
#' @keywords internal
#' @export
IV_center <- function (Z, weights) {
  N <- length(Z)
  Z_c <- Z - mean(Z * weights)
  eps_theta <- weights * Z_c / sum(weights)
  Z_model_mat <- matrix(rep(1, N), nrow = N, ncol = 1)
  iv_center <- list(Zc = Z_c, Z_model_mat = Z_model_mat, epstheta = eps_theta)
  return(iv_center)
}

#' This function is to generate a grid of treatment status
#' @keywords internal
#' @export
treatment_status <- function (N,
                              K,
                              stime, 
                              treatment_init, 
                              treatment_shift_time, 
                              max_time) {

  D_status <- matrix(0, ncol = K, nrow = N)
  for (i in 1:N) {
    if (treatment_shift_time[i] <= 0 || treatment_shift_time[i] >= max_time) {
      D_status[i, ] = treatment_init[i]
      next
    }
    D_status[i, stime < treatment_shift_time[i]] = treatment_init[i]
    D_status[i, stime >= treatment_shift_time[i]] = 1 - treatment_init[i]
  }
  return(D_status)
}



#' This function is to compute the centralized treatment process given the covariates Z, L at each sorted unique event time
#' @keywords internal
#' @export
trt_center <- function (D_status, Z, weights) {
  N <- nrow(D_status)
  K <- ncol(D_status)
  D_model_mat <- matrix(0, nrow = N, ncol = K * 2)
  eps_theta <- matrix(0, nrow = K * 2, ncol = N)
  Z_status <- matrix(rep(Z, K), ncol = K)
  pred_D_status <- t(colSums(D_status * Z * weights) / sum(Z * weights) * t(Z_status) + 
                       colSums(D_status * (1 - Z) * weights) / sum((1 - Z) * weights) * t(1 - Z_status))
  for (k in 1:K) {
    eps_theta[2 * (k - 1) + 1, ] <- 
      (1 - Z) * (D_status[, k] * weights - sum(D_status[, k] * (1 - Z) * weights) / sum((1 - Z) * weights)) / sum((1 - Z) * weights)
    eps_theta[2 * (k - 1) + 2, ] <- 
      Z * (D_status[, k] * weights - sum(D_status[, k] * Z * weights) / sum(Z * weights)) / sum(Z * weights)
    D_model_mat[, 2 * (k - 1) + 1] <- 1 - Z
    D_model_mat[, 2 * (k - 1) + 2] <- Z
  }
  D_status_c <- D_status - pred_D_status
  trt_center <- list(D_status_c = D_status_c, D_model_mat = D_model_mat, epstheta = eps_theta)
  return(trt_center)
}
