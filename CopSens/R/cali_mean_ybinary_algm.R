#' Calibrate Estimate of Intervention Mean for Binary Outcome
#'
#' @param i Observation index.
#' @param gamma Scalar or vector specifying the sensitivity parameters.
#' @param mu_u_tr Matrix of conditional confounder means for all observed treatments
#' with latent variables in columns.
#' @param mu_u_t Matrix of conditional confounder means for treatments of interest
#' with latent variables in columns.
#' @param mu_y_t Scalar or vector that contains naive estimates of treatment effects
#' ignoring confounding.
#' @param nsim Number of simulation sample draws.
#'
#' @return Scalar of calibrated intervention mean.
#'
#' @importFrom stats rnorm
#' @importFrom stats pnorm
#'
cali_mean_ybinary_algm <- function(i, gamma, mu_u_tr, mu_u_t, mu_y_t, nsim = 4000) {
  message(i, " ")
  mu_i <- c((mu_u_tr - mu_u_t[i,]) %*% gamma)
  ytilde_samples <- rnorm(n = nrow(mu_u_tr) * nsim, mean = mu_i, sd = 1)
  y_samples <- ifelse(pnorm(ytilde_samples) > 1 - mu_y_t[i], 1, 0)
  mean(y_samples)
}


