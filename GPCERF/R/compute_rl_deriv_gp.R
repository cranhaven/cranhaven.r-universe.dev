#' @title
#' Detect change-point in standard GP
#'
#' @description
#' Calculates the posterior mean of the difference between left- and
#' right-derivatives at an exposure level for the detection of change points.
#'
#' @param w A scalar of exposure level of interest.
#' @param w_obs A vector of observed exposure levels of all samples.
#' @param y_obs A vector of observed outcome values of all samples.
#' @param gps_m An S3 gps object including:
#'   gps: A data.frame of GPS vectors.
#'     - Column 1: GPS
#'     - Column 2: Prediction of exposure for covariate of each data sample
#'     (e_gps_pred).
#'     - Column 3: Standard deviation of  e_gps (e_gps_std)
#'   used_params:
#'     - dnorm_log: TRUE or FLASE
#' @param hyperparam A vector of hyper-parameters in the GP model.
#' @param kernel_fn The covariance function.
#' @param kernel_deriv_fn The partial derivative of the covariance function.
#'
#' @return
#' A numeric value of the posterior mean of the difference between two one-sided
#' derivatives.
#' @export
#'
#' @examples
#' \donttest{
#' set.seed(847)
#' data <- generate_synthetic_data(sample_size = 100)
#' gps_m <- estimate_gps(cov_mt = data[,-(1:2)],
#'                       w_all = data$treat,
#'                       sl_lib = c("SL.xgboost"),
#'                       dnorm_log = FALSE)
#'
#' wi <- 8.6
#'
#' val <- compute_rl_deriv_gp(w = wi,
#'                            w_obs = data$treat,
#'                            y_obs = data$Y,
#'                            gps_m = gps_m,
#'                            hyperparam = c(1,1,2))
#' }
compute_rl_deriv_gp <- function(w,
                                w_obs,
                                y_obs,
                                gps_m,
                                hyperparam,
                                kernel_fn = function(x) exp(-x),
                                kernel_deriv_fn = function(x) -exp(-x)) {



  # left side weights
  gps_m_left <- gps_m
  gps_m_left$gps <- gps_m_left$gps[w_obs < w, ]
  left_weights <-  compute_deriv_weights_gp(w = w,
                                            w_obs = w_obs[w_obs < w],
                                            gps_m = gps_m_left,
                                            hyperparam = hyperparam,
                                            kernel_fn = kernel_fn,
                                            kernel_deriv_fn = kernel_deriv_fn)

  # right side weights
  gps_m_right <- gps_m
  gps_m_right$gps <- gps_m_right$gps[w_obs >= w, ]
  right_weights <-  compute_deriv_weights_gp(w = w,
                                             w_obs = w_obs[w_obs >= w],
                                             gps_m = gps_m_right,
                                             hyperparam = hyperparam,
                                             kernel_fn = kernel_fn,
                                             kernel_deriv_fn = kernel_deriv_fn)

  # compute derivative
  return(right_weights %*% y_obs[w_obs >= w] -
         left_weights %*% y_obs[w_obs < w])
}
