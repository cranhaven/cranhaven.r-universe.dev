#' @title
#' Calculate weights for estimation of a point on CERF
#'
#' @description
#' Calculates the weights of observed outcomes which is then used to estimate
#' the posterior mean of CERF at a given exposure level.
#'
#' @param w A scalar of exposure level of interest.
#' @param w_obs A vector of observed exposure levels of all samples.
#' @param scaled_obs A matrix of two columns.
#'   - First column is the scaled GPS value of all samples
#'   (GPS * 1 / sqrt(alpha))
#'   - Second column is the scaled exposure value of all samples
#'   (w * 1/sqrt(beta))
#' @param hyperparam A vector of hyper-parameters for the GP.
#'   - First element: alpha
#'   - Second element: beta
#'   - Third element: gamma/sigma
#' @param inv_sigma_obs Inverse of the covariance matrix between observed
#' samples.
#' @param gps_m An S3 gps object including:
#'   gps: A data.frame of GPS vectors.
#'     - Column 1: GPS
#'     - Column 2: Prediction of exposure for covariate of each data sample
#'     (e_gps_pred).
#'     - Column 3: Standard deviation of  e_gps (e_gps_std)
#'   used_params:
#'     - dnorm_log: TRUE or FLASE
#' @param est_sd Should the posterior se be computed (default=FALSE)
#' @param kernel_fn The covariance function of GP.
#'
#' @return
#' A list of two elements, weight and standard deviation.
#'
#' @keywords internal
#'
compute_weight_gp <- function(w, w_obs, scaled_obs, hyperparam,
                              inv_sigma_obs, gps_m, est_sd = FALSE,
                              kernel_fn = function(x) exp(-x ^ 2)) {

  logger::log_trace("Computing weights for w = {w} ...")

  alpha <- hyperparam[[1]]
  beta <- hyperparam[[2]]
  g_sigma <- hyperparam[[3]]

  # Compute GPS for requested w
  e_gps_pred <- gps_m$gps$e_gps_pred
  e_gps_std <- gps_m$gps$e_gps_std
  dnorm_log <- gps_m$used_params$dnorm_log

  gps_w <- stats::dnorm(w, mean = e_gps_pred, sd = e_gps_std, log = dnorm_log)
  scaled_w <- cbind(w * sqrt(1 / beta), gps_w * sqrt(1 / alpha))
  colnames(scaled_w) <- c("w_sc_for_w", "gps_sc_for_w")

  # kappa
  # sigma_cross = kappa/sigma^2 : Is always n*n matrix.
  # each column of sigma_cross is ki.
  # statspat.geom::crossdist

  sigma_cross <- g_sigma * kernel_fn(crossdist(scaled_w[, "w_sc_for_w"],
                                               scaled_w[, "gps_sc_for_w"],
                                               scaled_obs[, "w_sc_obs"],
                                               scaled_obs[, "gps_sc_obs"]))

  logger::log_trace("sigma_cross {class(sigma_cross)[1]} ",
                    "({nrow(sigma_cross)}, {ncol(sigma_cross)}) ",
                    "was generated.")

  # each row is the weights for all subject for estimate of Y_i(w)
  # each column is the weight of an observed sample (w_i, c_i)
  normalized_sigma_cross <- Rfast::colmeans(sigma_cross)
  weight <- c(arma_mm(inv_sigma_obs, normalized_sigma_cross))

  logger::log_trace("Weight vector with size {length(weight)}",
                    " was generated.")

  # compute scaled posterior sd
  if (est_sd) {
    # TODO: It seems we are computing noise based on GPS value. Is that correct?
    # It is GPS.
    sigma_w <- g_sigma * kernel_fn(outer(scaled_w[, 2],
                                         scaled_w[, 2], "-") ^ 2) +
                                           diag(nrow(scaled_w))
    sd_scaled <- sqrt(sum(sigma_w) / nrow(scaled_w) ^ 2 -
                      sum(weight * normalized_sigma_cross))
    logger::log_trace("Computed scaled standard deviation: {sd_scaled}")
  } else {
    sd_scaled <- NA
  }

  return(list(weight = weight, sd_scaled = sd_scaled))
}
