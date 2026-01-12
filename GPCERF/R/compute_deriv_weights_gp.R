#' @title
#' Calculate derivatives of CERF
#'
#' @description
#' Calculates the weights assigned to each observed outcome when deriving the
#' posterior mean of the first derivative of CERF at a given exposure level.
#'
#' @param w A scalar of exposure level of interest.
#' @param w_obs A vector of observed exposure levels of all samples.
#' @param gps_m An S3 gps object including:
#'   gps: A data.frame of GPS vectors.
#'     - Column 1: GPS
#'     - Column 2: Prediction of exposure for covariate of each data sample
#'     (e_gps_pred).
#'     - Column 3: Standard deviation of  e_gps (e_gps_std)
#'   used_params:
#'     - dnorm_log: TRUE or FALSE
#' @param hyperparam A vector of hyper-parameters in the GP model.
#' @param kernel_fn The covariance function.
#' @param kernel_deriv_fn The partial derivative of the covariance function.
#'
#' @return
#' A vector of weights for all samples, based on which the posterior mean of
#' the derivative of CERF at the exposure level of interest is calculated.
#'
#' @keywords internal
#'
compute_deriv_weights_gp <- function(w,
                                     w_obs,
                                     gps_m,
                                     hyperparam,
                                     kernel_fn = function(x) exp(-x),
                                     kernel_deriv_fn = function(x) -exp(-x)) {


  alpha <- hyperparam[[1]]
  beta <- hyperparam[[2]]
  g_sigma <- hyperparam[[3]]


  gps <- gps_m$gps$gps
  e_gps_pred <- gps_m$gps$e_gps_pred
  e_gps_std <- gps_m$gps$e_gps_std
  dnorm_log <- gps_m$used_params$dnorm_log

  gps_w <- dnorm(w, mean = e_gps_pred, sd = e_gps_std, log = dnorm_log)
  n <- length(gps_w)

  obs_use <- cbind(w_obs * sqrt(1 / beta), gps * sqrt(1 / alpha))
  colnames(obs_use) <- c("w_sc_obs", "gps_sc_obs")

  obs_new <- cbind(w * sqrt(1 / beta), gps_w * sqrt(1 / alpha))
  colnames(obs_new) <- c("w_sc_for_w", "gps_sc_for_w")

  sigma_obs <- g_sigma * kernel_fn(as.matrix(dist(obs_use)) ^ 2) +
               diag(nrow(obs_use))
  cross_dist <- spatstat.geom::crossdist(obs_new[, "w_sc_for_w"],
                                         obs_new[, "gps_sc_for_w"],
                                         obs_use[, "w_sc_obs"],
                                         obs_use[, "gps_sc_obs"])

  sigma_cross <- g_sigma * sqrt(1 / beta) * kernel_deriv_fn(cross_dist ^ 2) *
                         (2 * outer(rep(w, n), w_obs, "-"))
  weights_all <- sigma_cross %*% chol2inv(chol(sigma_obs))

  return(colMeans(weights_all))
}
