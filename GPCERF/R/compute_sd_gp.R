#' @title
#' Compute posterior credible interval
#'
#' @description
#' Computes posterior credible interval for requested exposure level.
#'
#' @param w A scalar of exposure level of interest.
#' @param scaled_obs A matrix of two columns.
#'   - First column is the scaled GPS value of all samples (GPS * 1/sqrt(alpha))
#'   - Second column is the scaled exposure value of all samples
#'   (w * 1/sqrt(beta))
#' @param hyperparam A vector of hyper-parameters for the GP.
#'   - First element: alpha
#'   - Second element: beta
#'   - Third element: gamma/sigma
#' @param sigma  A scaler that represents noise.
#' @param gps_m An S3 gps object including:
#'   gps: A data.frame of GPS vectors.
#'     - Column 1: GPS
#'     - Column 2: Prediction of exposure for covariate of each data sample
#'     (e_gps_pred).
#'     - Column 3: Standard deviation of  e_gps (e_gps_std)
#'   used_params:
#'     - dnorm_log: TRUE or FLASE
#' @param kernel_fn The covariance function of GP.
#'
#' @keywords internal
#'
#' @return
#' Posterior credible interval (scaler) for the requested exposure level (w).
#'
compute_sd_gp <- function(w,
                          scaled_obs,
                          hyperparam,
                          sigma,
                          gps_m,
                          kernel_fn = function(x) exp(-x ^ 2)) {


  alpha <- hyperparam[[1]]
  beta <- hyperparam[[2]]
  g_sigma <- hyperparam[[3]]

  n <- nrow(scaled_obs)

  # Compute GPS for requested w
  e_gps_pred <- gps_m$gps$e_gps_pred
  e_gps_std <- gps_m$gps$e_gps_std
  dnorm_log <- gps_m$used_params$dnorm_log
  gps_w <- stats::dnorm(w, mean = e_gps_pred, sd = e_gps_std, log = dnorm_log)

  # Compute helper matrix for the new w and corresponding GPS.
  scaled_w <- cbind(w / sqrt(1 / beta), gps_w / sqrt(1 / alpha))

  scaled_combined <- rbind(scaled_w, scaled_obs)
  Sigma_all <- (g_sigma * kernel_fn(as.matrix(stats::dist(scaled_combined))) +
                diag(n * 2)) * sigma ^ 2
  Sigma_within_w <- Sigma_all[1:n, 1:n]
  Sigma_cross <- Sigma_all[1:n, -(1:n)]
  Sigma_within_obs <- Sigma_all[-(1:n), -(1:n)]
  Sigma_conditional <- Sigma_within_w -
                       Sigma_cross %*%
                       chol2inv(chol(Sigma_within_obs)) %*%
                       t(Sigma_cross)
  posterior_sd <- sqrt(rep(1 / n, n) %*%
                  Sigma_conditional %*%
                  rep(1 / n, n) + sigma ^ 2)
  return(posterior_sd)
}
