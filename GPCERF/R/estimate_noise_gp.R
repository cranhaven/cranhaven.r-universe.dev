#' @title
#' Estimate the standard deviation of the nugget term in standard Gaussian
#' process
#'
#' @description
#' Estimates the standard deviations of the nugget term in standard GP by
#' calculating the standard deviations of the residuals.
#'
#' @param data A vector of outcome data.
#' @param sigma_obs Covariance matrix between observed covariates.
#' @param inv_sigma_obs Inverse of the covariance matrix between observed
#' covariates.
#'
#' @return
#' A scalar of estimated standard deviation of the nugget term in standard GP.
#'
#' @keywords internal
#'
estimate_noise_gp <- function(data, sigma_obs, inv_sigma_obs) {

  noise <- sd(data - arma_mm(sigma_obs - diag(nrow(sigma_obs)),
                               arma_mm(inv_sigma_obs, data)))

  return(noise)
}
