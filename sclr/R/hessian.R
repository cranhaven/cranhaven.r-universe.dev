# Second derivative matrix
# Arseniy Khvorov
# Created 2019/11/01
# Last edit 2019/11/01

#' Second derivative matrix (reparameterised)
#'
#' @param y Model response
#' @param x Model matrix
#' @param pars_mat Parameter matrix
#' @param exp_Xb exp(xb) term
#' @param x_coeffs Matrix of pairwise products of elements of x
#'
#' @noRd
get_hessian <- function(y, x, pars_mat, exp_Xb, x_coeffs) {
  
  lambda_entries <- get_theta_array(y, x, pars_mat, exp_Xb)
  beta_part <- get_hes_beta_part(y, x, pars_mat, exp_Xb, x_coeffs)
  
  n_x <- ncol(x)
  
  # Convert the beta part to a matrix
  beta_part_mat <- build_symm_mat(beta_part)
  
  # Combine into full matrix
  n_par <- n_x + 1
  
  sec_dev_mat <- matrix(0, nrow = n_par, ncol = n_par)
  sec_dev_mat[1, ] <- lambda_entries
  sec_dev_mat[, 1] <- lambda_entries
  sec_dev_mat[2:n_par, 2:n_par] <- beta_part_mat
  
  sec_dev_mat
}

#' lambda row and column entries
#'
#' @param y Model response
#' @param x Model matrix
#' @param pars_mat Parameter matrix
#' @param exp_Xb exp(xb) term
#'
#' @noRd
get_theta_array <- function(y, x, pars_mat, exp_Xb) {
  dtheta2 <- get_dtheta2(y, x, pars_mat, exp_Xb)
  dthetadbs <- get_dthetadbs(y, x, pars_mat, exp_Xb)
  c(dtheta2, dthetadbs)
}

#' Second derivative of log-likelihood with respect to lambda
#'
#' @inheritParams get_theta_array
#'
#' @noRd
get_dtheta2 <- function(y, x, pars_mat, exp_Xb) {
  theta <- pars_mat[1, ]
  dtheta2 <- -exp(theta) / (1 + exp(theta))^2 + 
    (1 - y) * (1 + exp_Xb) * exp_Xb * exp(theta) / 
    (1 + exp_Xb * (1 + exp(theta)))^2
  sum(dtheta2)
}

#' Second derivative of log-likelihood with respect to lambda and betas
#' (reparameterised)
#'
#' @inheritParams get_theta_array
#'
#' @noRd
get_dthetadbs <- function(y, x, pars_mat, exp_Xb) {
  theta <- pars_mat[1, ]
  common <- (1 - y) * exp_Xb * exp(theta) / (1 + exp_Xb * (1 + exp(theta)))^2
  dldbs_contr <- x * common[, 1]
  colSums(dldbs_contr)
}

#' Second derivative of log-likelihood with respect to betas
#'
#' @inheritParams get_hessian
#'
#' @noRd
get_hes_beta_part <- function(y, x, pars_mat, exp_Xb, x_coeffs) {
  theta <- pars_mat[1, ]
  common <- -exp_Xb / (1 + exp_Xb)^2 + (1 - y) * (1 + exp(theta)) * exp_Xb /
    (1 + exp_Xb * (1 + exp(theta)))^2
  
  # Get the unique triangle of the beta part
  beta_contr <- x_coeffs * common[, 1]
  colSums(beta_contr)
}
