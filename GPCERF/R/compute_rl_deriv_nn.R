#' @title
#' Calculate right minus left derivatives for change-point detection in nnGP
#'
#' @description
#' Calculates the posterior mean of the difference between left- and
#' right-derivatives at an exposure level for the detection of change points.
#' nnGP approximation is used.
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
#'     - dnorm_log: TRUE or FLASE
#' @param y_obs A vector of observed outcome values.
#' @param hyperparam A vector of hyper-parameters in the GP model.
#' @param n_neighbor The number of nearest neighbors on one side.
#' @param block_size The number of samples included in a computation block.
#' Mainly used to balance the speed and memory requirement. Larger
#' \code{block_size} is faster, but requires more memory.
#' @param kernel_fn The covariance function. The input is the square of
#' Euclidean distance.
#' @param kernel_deriv_fn The partial derivative of the covariance function.
#' The input is the square of Euclidean distance.
#'
#' @return
#' A numeric value of the posterior mean of the difference between two one-sided
#' derivatives.
#'
#' @export
#'
#' @examples
#' \donttest{
#' set.seed(325)
#' data <- generate_synthetic_data(sample_size = 200)
#' gps_m <- estimate_gps(cov_mt = data[,-(1:2)],
#'                       w_all = data$treat,
#'                       sl_lib = c("SL.xgboost"),
#'                       dnorm_log = FALSE)
#'
#' wi <- 12.2
#'
#' deriv_val <- compute_rl_deriv_nn(w = wi,
#'                                  w_obs = data$treat,
#'                                  gps_m = gps_m,
#'                                  y_obs = data$Y,
#'                                  hyperparam = c(0.2,0.4,1.2),
#'                                  n_neighbor = 20,
#'                                  block_size = 10)
#'}
compute_rl_deriv_nn <-  function(w,
                                 w_obs,
                                 gps_m,
                                 y_obs,
                                 hyperparam,
                                 n_neighbor,
                                 block_size,
                                 kernel_fn = function(x) exp(-x),
                                 kernel_deriv_fn = function(x) -exp(-x)
                                 ) {

  gps_m_left <- gps_m
  gps_m_left$gps <- gps_m_left$gps[w_obs < w, ]
  left_deriv <- compute_deriv_nn(w,
                                 w_obs[w_obs < w],
                                 gps_m_left,
                                 y_obs[w_obs < w],
                                 hyperparam,
                                 n_neighbor = n_neighbor,
                                 block_size = block_size,
                                 kernel_fn = kernel_fn,
                                 kernel_deriv_fn = kernel_deriv_fn)


  gps_m_right <- gps_m
  gps_m_right$gps <- gps_m_right$gps[w_obs >= w, ]
  right_deriv <- compute_deriv_nn(w,
                                  w_obs[w_obs >= w],
                                  gps_m_right,
                                  y_obs[w_obs >= w],
                                  hyperparam,
                                  n_neighbor = n_neighbor,
                                  block_size = block_size,
                                  kernel_fn = kernel_fn,
                                  kernel_deriv_fn = kernel_deriv_fn)

  return(right_deriv - left_deriv)
}
